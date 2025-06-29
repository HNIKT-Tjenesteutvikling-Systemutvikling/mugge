package chat

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.*
import fs2.io.net.*
import fs2.Stream
import com.comcast.ip4s.*
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.io.IOException
import scala.concurrent.duration.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => TLogger}

object ChatServer extends IOApp:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  private val port = port"5555"
  private val host = host"0.0.0.0"

  private type KeyCache = Map[String, (List[Authentication.PublicKeyInfo], Long)]

  def run(args: List[String]): IO[ExitCode] =
    for
      _ <- logger.info(s"Starting chat server on $host:$port")
      clients <- Ref.of[IO, Map[String, Client]](Map.empty)
      keyCache <- Ref.of[IO, KeyCache](Map.empty)
      challenges <- Ref.of[IO, Map[String, (String, String)]](
        Map.empty
      )
      _ <- Network[IO]
        .server(Some(host), Some(port))
        .flatMap { clientSocket =>
          Stream.eval(
            handleClient(clientSocket, clients, keyCache, challenges)
              .handleErrorWith(err => logger.error(s"Client error: ${err.getMessage}"))
          )
        }
        .compile
        .drain
    yield ExitCode.Success

  private def handleClient(
      socket: Socket[IO],
      clients: Ref[IO, Map[String, Client]],
      keyCache: Ref[IO, KeyCache],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Unit] =
    for
      initialDataChunk <- socket.read(8192)
      initialData <- IO.fromOption(initialDataChunk)(new IOException("Client disconnected"))
      dataString = initialData.toArray.map(_.toChar).mkString

      lines = dataString.split("\n").filter(_.nonEmpty)
      hostname = lines.headOption.getOrElse("unknown")
      autoAuthData = lines.find(_.startsWith("auto-auth:"))

      clientId = UserMapping.mapHostname(hostname)
      queue <- Queue.unbounded[IO, Message]

      authenticated <- autoAuthData match
        case Some(authLine) =>
          val githubUsername = authLine.drop(10).trim
          for
            _ <- logger.debug(s"Client $clientId attempting auto-auth as $githubUsername")
            challenge <- Authentication.generateChallenge().flatMap(IO.fromEither)
            _ <- challenges.update(_ + (clientId -> (challenge, githubUsername)))
            _ <- socket.write(Chunk.from(s"CHALLENGE:$challenge\n".getBytes))
            result <- IO.race(
              IO.sleep(5.seconds).as(false),
              waitForAutoAuthResponse(
                socket,
                clientId,
                githubUsername,
                challenge,
                keyCache,
                challenges
              )
            )
          yield result.getOrElse(false)
        case None =>
          IO.pure(false)

      client = Client(clientId, queue, socket, authenticated, autoAuthData.map(_.drop(10).trim))
      _ <- clients.update(_ + (clientId -> client))
      _ <- logger.debug(s"Client $clientId connected (authenticated: $authenticated)")

      welcomeMsg =
        if authenticated then
          s"Welcome! You are authenticated as ${client.githubUsername.getOrElse("unknown")}\n"
        else s"Welcome! Your ID is: $clientId\nPlease authenticate with: /auth <github-username>\n"

      _ <- socket.write(Chunk.from(welcomeMsg.getBytes))
      _ <- (
        Stream
          .eval(readFromClient(client, clients, keyCache, challenges))
          .concurrently(
            Stream.eval(writeToClient(client))
          )
        )
        .compile
        .drain
        .guarantee(
          removeClient(clientId, clients, challenges)
        )
    yield ()

  private def waitForAutoAuthResponse(
      socket: Socket[IO],
      clientId: String,
      githubUsername: String,
      challenge: String,
      keyCache: Ref[IO, KeyCache],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Boolean] =
    for
      responseChunk <- socket.read(8192)
      response <- IO.fromOption(responseChunk)(new IOException("No auth response"))
      responseStr = response.toArray.map(_.toChar).mkString.trim
      result <-
        if responseStr.startsWith("SIGNATURE:") then
          val signature = responseStr.drop(10)
          verifyAuthSignature(clientId, githubUsername, challenge, signature, keyCache)
        else IO.pure(false)
      _ <- challenges.update(_ - clientId)
    yield result

  private def verifyAuthSignature(
      clientId: String,
      githubUsername: String,
      challenge: String,
      signature: String,
      keyCache: Ref[IO, KeyCache]
  ): IO[Boolean] =
    for
      keys <- getCachedKeys(githubUsername, keyCache)
      results <- keys.traverse { keyInfo =>
        Authentication.verifySignature(challenge, signature, keyInfo.key).map(_.getOrElse(false))
      }
      verified = results.exists(identity)
      _ <-
        if verified then
          logger.debug(s"Successfully verified signature for $clientId as $githubUsername")
        else logger.error(s"Failed to verify signature for $clientId as $githubUsername")
    yield verified

  private def readFromClient(
      client: Client,
      clients: Ref[IO, Map[String, Client]],
      keyCache: Ref[IO, KeyCache],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Unit] =
    client.socket.reads
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .evalMap { line =>
        if line.startsWith("/auth ") then handleAuth(line.drop(6), client, keyCache, challenges)
        else if line.startsWith("/verify ") then
          handleVerify(line.drop(8), client, clients, keyCache, challenges)
        else if client.authenticated then
          val message = Message(
            timestamp = LocalDateTime.now(),
            clientId = client.id,
            content = line,
            authenticated = true
          )
          broadcastMessage(message, clients)
        else sendToClient(client, "You must authenticate first. Use: /auth <github-username>")
      }
      .compile
      .drain

  private def handleAuth(
      githubUsername: String,
      client: Client,
      keyCache: Ref[IO, KeyCache],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Unit] =
    val isAuthorized = Authentication.AuthorizedUser.values
      .exists(_.githubUsername.equalsIgnoreCase(githubUsername))

    if !isAuthorized then
      sendToClient(client, s"User $githubUsername is not authorized to use this chat.")
    else
      for
        _ <- logger.debug(s"${client.id} attempting to authenticate as $githubUsername")
        keys <- getCachedKeys(githubUsername, keyCache)
        _ <-
          if keys.isEmpty then
            sendToClient(client, s"No SSH keys found for GitHub user: $githubUsername")
          else
            for
              challenge <- Authentication.generateChallenge().flatMap(IO.fromEither)
              _ <- challenges.update(_ + (client.id -> (challenge, githubUsername)))
              _ <- sendToClient(client, s"Challenge: $challenge")
              _ <- sendToClient(
                client,
                "Please sign this challenge and respond with: /verify <signature>"
              )
            yield ()
      yield ()

  private def handleVerify(
      signature: String,
      client: Client,
      clients: Ref[IO, Map[String, Client]],
      keyCache: Ref[IO, KeyCache],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Unit] =
    for
      challengeMap <- challenges.get
      maybeChallengeData = challengeMap.get(client.id)
      _ <- maybeChallengeData match
        case None =>
          sendToClient(client, "No active authentication challenge. Use: /auth <github-username>")
        case Some((challenge, githubUsername)) =>
          for
            verified <- verifyAuthSignature(
              client.id,
              githubUsername,
              challenge,
              signature,
              keyCache
            )
            _ <-
              if verified then
                for
                  _ <- clients.update(
                    _.updatedWith(client.id)(
                      _.map(
                        _.copy(
                          authenticated = true,
                          githubUsername = Some(githubUsername)
                        )
                      )
                    )
                  )
                  _ <- sendToClient(client, s"Authentication successful! Welcome, $githubUsername!")
                  _ <- challenges.update(_ - client.id)
                yield ()
              else sendToClient(client, "Authentication failed. Invalid signature.")
          yield ()
    yield ()

  private def getCachedKeys(
      githubUsername: String,
      keyCache: Ref[IO, KeyCache]
  ): IO[List[Authentication.PublicKeyInfo]] =
    for
      now <- IO.realTime.map(_.toMillis)
      cache <- keyCache.get
      keys <- cache.get(githubUsername) match
        case Some((keys, timestamp)) if (now - timestamp) < Authentication.CACHE_TIMEOUT_MS =>
          IO.pure(keys)
        case _ =>
          for
            _ <- logger.debug(s"Fetching SSH keys for $githubUsername from GitHub")
            freshKeys <- Authentication
              .fetchGithubKeys(githubUsername)
              .flatMap(IO.fromEither)
              .handleErrorWith { err =>
                logger.error(s"Error fetching keys: ${err.getMessage}") *>
                  IO.pure(List.empty[Authentication.PublicKeyInfo])
              }
            _ <- keyCache.update(_ + (githubUsername -> (freshKeys, now)))
          yield freshKeys
    yield keys

  private def sendToClient(client: Client, message: String): IO[Unit] =
    val serverMessage = Message(
      timestamp = LocalDateTime.now(),
      clientId = "SERVER",
      content = message,
      authenticated = true
    )
    client.queue.offer(serverMessage).handleErrorWith(_ => IO.unit)

  private def writeToClient(client: Client): IO[Unit] =
    Stream
      .fromQueueUnterminated(client.queue)
      .map(formatMessage)
      .through(text.utf8.encode)
      .through(client.socket.writes)
      .compile
      .drain

  private def formatMessage(message: Message): String =
    val timeStr = message.timestamp.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
    val authIndicator = if message.authenticated then "✓" else "?"
    s"[$timeStr] $authIndicator ${message.clientId}: ${message.content}\n"

  private def broadcastMessage(message: Message, clients: Ref[IO, Map[String, Client]]): IO[Unit] =
    for
      currentClients <- clients.get
      _ <- currentClients.values.toList.traverse_ { client =>
        if client.authenticated then client.queue.offer(message).handleErrorWith(_ => IO.unit)
        else IO.unit
      }
      _ <- IO.println(formatMessage(message).trim)
    yield ()

  private def removeClient(
      clientId: String,
      clients: Ref[IO, Map[String, Client]],
      challenges: Ref[IO, Map[String, (String, String)]]
  ): IO[Unit] =
    for
      _ <- clients.update(_ - clientId)
      _ <- challenges.update(_ - clientId)
      _ <- IO.println(s"Client $clientId disconnected")
      remainingClients <- clients.get
      disconnectMessage = Message(
        timestamp = LocalDateTime.now(),
        clientId = "SERVER",
        content = s"Client $clientId has left the chat",
        authenticated = true
      )
      _ <-
        if remainingClients.nonEmpty then broadcastMessage(disconnectMessage, clients)
        else IO.unit
    yield ()
