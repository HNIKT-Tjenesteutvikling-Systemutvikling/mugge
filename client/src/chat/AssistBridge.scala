package chat

import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Queue
import cats.mtl.Handle.allow
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.*
import fs2.io.net.*
import org.typelevel.log4cats.Logger as TLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.security.PrivateKey
import java.util.Base64

/** `--assist <user>` mode: pipes stdin/stdout of this process to a consenting user's shell. */
object AssistBridge:
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  def run(target: String, insecureTls: Boolean): IO[ExitCode] =
    val host =
      sys.env.get("CHAT_SERVER_HOST").flatMap(Host.fromString).getOrElse(Config.defaultHost)
    val port =
      sys.env.get("CHAT_SERVER_PORT").flatMap(Port.fromString).getOrElse(Config.defaultPort)
    for
      _ <- IO.whenA(insecureTls)(IO.println(Config.insecureTlsNotice))
      githubUsername <- allow[Authentication.AuthError] {
        Authentication.detectGithubUsername()
      }.rescue(_ => IO.pure(None))
      privateKey <- allow[Authentication.AuthError] {
        Authentication.loadPrivateKey().map(_.some)
      }.rescue(_ => IO.pure(None))
      code <- (githubUsername, privateKey) match
        case (Some(gh), Some(key)) =>
          (if insecureTls then Network[IO].tlsContext.insecure else Tls.pinnedContext)
            .flatMap { tls =>
              Network[IO]
                .connect(SocketAddress(host, port))
                .use(raw =>
                  tls
                    .clientBuilder(raw)
                    .withParameters(Tls.parameters)
                    .build
                    .use(socket => session(socket, gh, key, target))
                )
            }
            .handleErrorWith { err =>
              if Tls.isPinMismatch(err) then
                Console[IO].errorln(Config.pinMismatchNotice).as(ExitCode.Success)
              else logger.error(s"Bridge connection failed: ${err.getMessage}").as(ExitCode.Error)
            }
        case _ =>
          logger
            .error("Bridge requires a GitHub username and SSH key (admin machine).")
            .as(ExitCode.Error)
    yield code

  private def session(
      socket: Socket[IO],
      githubUsername: String,
      privateKey: PrivateKey,
      target: String
  ): IO[ExitCode] =
    for
      hostname <- Config.hostname
      outQ <- Queue.unbounded[IO, String]
      done <- Deferred[IO, ExitCode]
      stdinFib <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      _ <- outQ.offer(
        s"$hostname\nproto:${Config.protocolVersion}\nauto-auth:$githubUsername\nassist-bridge:1\n\n"
      )
      writer = Stream
        .fromQueueUnterminated(outQ)
        .through(text.utf8.encode)
        .through(socket.writes)
      reader = socket.reads
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty)
        .evalMap(line => handleLine(line, outQ, done, stdinFib, privateKey, target))
        .onFinalize(done.complete(ExitCode.Error).attempt.void)
      pinger = Stream.awakeEvery[IO](Config.pingInterval).evalMap(_ => outQ.offer("PING\n"))
      pumps <- Stream(reader.drain, writer.drain, pinger.drain).parJoinUnbounded.compile.drain.start
      code <- done.get
        .guarantee(pumps.cancel *> stdinFib.get.flatMap(_.traverse_(_.cancel)))
    yield code

  private def handleLine(
      line: String,
      outQ: Queue[IO, String],
      done: Deferred[IO, ExitCode],
      stdinFib: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
      privateKey: PrivateKey,
      target: String
  ): IO[Unit] =
    if line.startsWith("CHALLENGE:") then
      allow[Authentication.AuthError] {
        Authentication
          .signChallenge(line.drop("CHALLENGE:".length), privateKey)
          .flatMap(sig => outQ.offer(s"SIGNATURE:$sig\n"))
      }.rescue { err =>
        logger.error(s"Bridge auth signing failed: ${err.message}") *>
          done.complete(ExitCode.Error).void
      }
    else if line == "ASSISTREADY" then outQ.offer(s"ASSIST:$target\n")
    else if line.startsWith("ASSISTOK:") then
      startPump(line.drop("ASSISTOK:".length).trim, outQ, done, stdinFib)
    else if line.startsWith("ASSISTERR:") then
      logger.error(s"Assist refused: ${line.drop("ASSISTERR:".length).trim}") *>
        done.complete(ExitCode.Error).void
    else if line.startsWith("ASSISTDATA:") then
      line.split(":", 3) match
        case Array(_, _, b64) => IO(Base64.getDecoder.decode(b64)).flatMap(writeStdout)
        case _                => IO.unit
    else if line.startsWith("ASSISTEND:") then done.complete(ExitCode.Success).void
    else if line.startsWith("INCOMPATIBLE:") then
      Console[IO].errorln(line.drop("INCOMPATIBLE:".length)) *> done.complete(ExitCode.Error).void
    else if line.startsWith("DENIED:") then
      Console[IO].errorln(line.drop("DENIED:".length)) *> done.complete(ExitCode.Success).void
    else IO.unit

  private def startPump(
      id: String,
      outQ: Queue[IO, String],
      done: Deferred[IO, ExitCode],
      stdinFib: Ref[IO, Option[Fiber[IO, Throwable, Unit]]]
  ): IO[Unit] =
    fs2.io
      .stdin[IO](64 * 1024)
      .chunks
      .evalMap(chunk =>
        outQ.offer(s"ASSISTDATA:$id:${Base64.getEncoder.encodeToString(chunk.toArray)}\n")
      )
      .compile
      .drain
      .flatMap(_ => outQ.offer(s"ASSISTEND:$id\n") *> done.complete(ExitCode.Success).void)
      .attempt
      .void
      .start
      .flatMap(fib => stdinFib.set(Some(fib)))

  private def writeStdout(bytes: Array[Byte]): IO[Unit] =
    IO.blocking { System.out.write(bytes); System.out.flush() }
