package chat

import cats.effect.*
import cats.syntax.all.*
import cats.data.EitherT
import java.security.*
import java.security.spec.*
import java.util.Base64
import java.nio.file.{Files, Paths, LinkOption}
import scala.sys.process.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger => TLogger}

object Authentication:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given logger: TLogger[IO] = Slf4jLogger.getLogger[IO]

  sealed trait AuthError extends Exception:
    def message: String
    override def getMessage: String = message

  case class KeyParseError(message: String) extends AuthError
  case class KeyLoadError(message: String) extends AuthError
  case class KeyConversionError(message: String) extends AuthError
  case class SignatureError(message: String) extends AuthError
  case class GitConfigError(message: String) extends AuthError

  enum AuthorizedUser(val githubUsername: String):
    case Gako358 extends AuthorizedUser("Gako358")
    case Merrinx extends AuthorizedUser("merrinx")
    case Grindstein extends AuthorizedUser("grindstein")
    case Ievensen extends AuthorizedUser("ievensen")
    case Neethan extends AuthorizedUser("neethan")
    case Sigubrat extends AuthorizedUser("sigubrat")
    case Intervbs extends AuthorizedUser("intervbs")
    case Jca002 extends AuthorizedUser("jca002")
    case KristianAN extends AuthorizedUser("KristianAN")
    case Solheim extends AuthorizedUser("5olheim")
    case TurboNaepskrel extends AuthorizedUser("TurboNaepskrel")

  case class AuthChallenge(challenge: String, signature: Option[String])

  def loadPrivateKey(
      keyPath: String = s"${System.getProperty("user.home")}/.ssh/id_rsa"
  ): IO[Either[AuthError, PrivateKey]] =
    for {
      home <- IO(System.getProperty("user.home"))
      keyPaths = List(
        keyPath,
        s"$home/.config/sops-nix/secrets/private_keys/gako",
        s"$home/.ssh/id_rsa",
        s"$home/.ssh/id_ed25519",
        s"$home/.ssh/id_ecdsa"
      )
      existingKeyPath <- IO {
        keyPaths
          .find(path => Files.exists(Paths.get(path)))
          .toRight(
            KeyLoadError(
              s"No SSH keys found in any of the following locations: ${keyPaths.mkString(", ")}"
            )
          )
      }.flatTap {
        case Right(path) => logger.debug(s"Found key at: $path")
        case Left(_)     => IO.unit
      }
      result <- existingKeyPath match {
        case Left(err) => IO.pure(Left(err))
        case Right(path) =>
          for {
            keyBytes <- IO.blocking {
              Either
                .catchNonFatal(Files.readAllBytes(Paths.get(path)))
                .leftMap(e => KeyLoadError(s"Failed to read key file: ${e.getMessage}"))
            }
            _ <- logger.debug(s"Reading key from: $path")
            keyString = keyBytes.map(new String(_))
            actualPath <- IO.blocking {
              Either
                .catchNonFatal(Paths.get(path).toRealPath())
                .leftMap(e => KeyLoadError(s"Failed to resolve key path: ${e.getMessage}"))
            }
            _ <- actualPath match {
              case Right(ap) if ap.toString != path =>
                logger.debug(s"Key is symlinked to: $ap")
              case _ => IO.unit
            }
            _ <- keyString match {
              case Right(str) => logger.debug(s"Key format detected: ${detectKeyFormat(str)}")
              case _          => IO.unit
            }
            privateKey <- (for {
              str <- EitherT.fromEither[IO](keyString)
              pk <- parsePrivateKey(path, str)
            } yield pk).value
          } yield privateKey
      }
    } yield result

  private def detectKeyFormat(keyString: String): String =
    if keyString.contains("BEGIN OPENSSH PRIVATE KEY") then "OpenSSH"
    else if keyString.contains("BEGIN RSA PRIVATE KEY") then "PKCS1"
    else if keyString.contains("BEGIN PRIVATE KEY") then "PKCS8"
    else if keyString.contains("BEGIN EC PRIVATE KEY") then "EC"
    else "Unknown"

  private def parsePrivateKey(path: String, keyString: String): EitherT[IO, AuthError, PrivateKey] =
    if keyString.contains("BEGIN OPENSSH PRIVATE KEY") then convertOpenSSHKey(keyString)
    else if keyString.contains("BEGIN RSA PRIVATE KEY") then parsePKCS1Key(keyString)
    else if keyString.contains("BEGIN PRIVATE KEY") then
      EitherT.fromEither[IO] {
        Either
          .catchNonFatal {
            val pemContent = keyString
              .replace("-----BEGIN PRIVATE KEY-----", "")
              .replace("-----END PRIVATE KEY-----", "")
              .replaceAll("\\s", "")

            val decoded = Base64.getDecoder.decode(pemContent)
            val keySpec = new PKCS8EncodedKeySpec(decoded)
            val keyFactory = KeyFactory.getInstance("RSA")
            keyFactory.generatePrivate(keySpec)
          }
          .leftMap(e => KeyParseError(s"Failed to parse PKCS8 key: ${e.getMessage}"))
      }
    else if keyString.contains("BEGIN EC PRIVATE KEY") then
      EitherT.fromEither[IO](
        Left(KeyLoadError("EC keys are not supported. Please use an RSA key."))
      )
    else EitherT.fromEither[IO](Left(KeyLoadError(s"Unknown key format in $path")))

  private def convertOpenSSHKey(
      keyString: String
  ): EitherT[IO, AuthError, PrivateKey] =
    EitherT(IO.blocking {
      Either
        .catchNonFatal {
          val tempFile = Files.createTempFile("ssh_key", ".pem")
          val tempFileOut = Files.createTempFile("ssh_key_out", ".pem")

          Files.write(tempFile, keyString.getBytes)

          val cmd =
            Seq("ssh-keygen", "-p", "-m", "PEM", "-f", tempFile.toString, "-P", "", "-N", "")
          val result = cmd.!

          val privateKey = if result == 0 then
            val convertedKey = Files.readString(tempFile)
            Files.delete(tempFile)
            Files.delete(tempFileOut)

            if convertedKey.contains("BEGIN RSA PRIVATE KEY") then parsePKCS1KeySync(convertedKey)
            else Left(KeyConversionError("Conversion did not produce PKCS1 format"))
          else
            val opensslCmd =
              Seq("ssh-keygen", "-p", "-m", "PKCS8", "-f", tempFile.toString, "-P", "", "-N", "")
            val opensslResult = opensslCmd.!

            if opensslResult == 0 then
              val convertedKey = Files.readString(tempFile)
              Files.delete(tempFile)
              Files.delete(tempFileOut)

              val pemContent = convertedKey
                .replace("-----BEGIN PRIVATE KEY-----", "")
                .replace("-----END PRIVATE KEY-----", "")
                .replaceAll("\\s", "")

              val decoded = Base64.getDecoder.decode(pemContent)
              val keySpec = new PKCS8EncodedKeySpec(decoded)
              val keyFactory = KeyFactory.getInstance("RSA")
              Right(keyFactory.generatePrivate(keySpec))
            else Left(KeyConversionError("Both ssh-keygen conversions failed"))

          privateKey
        }
        .leftMap(e => KeyConversionError(s"Failed to convert OpenSSH key: ${e.getMessage}"))
        .flatten
    })

  private def parsePKCS1Key(keyString: String): EitherT[IO, AuthError, PrivateKey] =
    EitherT(IO.blocking(parsePKCS1KeySync(keyString)))

  private def parsePKCS1KeySync(keyString: String): Either[AuthError, PrivateKey] =
    import scala.sys.process._

    Either
      .catchNonFatal {
        val tempIn = Files.createTempFile("key", ".pem")
        val tempOut = Files.createTempFile("key", ".der")

        Files.write(tempIn, keyString.getBytes)

        val result =
          s"openssl pkcs8 -topk8 -inform PEM -outform DER -in $tempIn -out $tempOut -nocrypt".!

        if result == 0 then
          val keyBytes = Files.readAllBytes(tempOut)
          val keySpec = new PKCS8EncodedKeySpec(keyBytes)
          val keyFactory = KeyFactory.getInstance("RSA")
          val privateKey = keyFactory.generatePrivate(keySpec)

          Files.delete(tempIn)
          Files.delete(tempOut)

          privateKey
        else throw new Exception("OpenSSL conversion failed")
      }
      .leftMap(e => KeyConversionError(s"Failed to convert PKCS1 key: ${e.getMessage}"))

  def signChallenge(challenge: String, privateKey: PrivateKey): IO[Either[AuthError, String]] =
    EitherT
      .fromEither[IO] {
        Either
          .catchNonFatal {
            val signature = Signature.getInstance("SHA256withRSA")
            signature.initSign(privateKey)
            signature.update(challenge.getBytes("UTF-8"))
            val signatureBytes = signature.sign()
            Base64.getEncoder.encodeToString(signatureBytes)
          }
          .leftMap(e => SignatureError(s"Failed to sign challenge: ${e.getMessage}"))
      }
      .value

  def detectGithubUsername(): IO[Either[AuthError, Option[String]]] =
    EitherT(IO.blocking {
      Either
        .catchNonFatal {
          scala.sys.process.Process(Seq("git", "config", "--global", "user.name")).!!.trim
        }
        .leftMap(e => GitConfigError(s"Failed to read git config: ${e.getMessage}"))
        .map { gitConfig =>
          val username = gitConfig.toLowerCase match {
            case "merrinx" => "Gako358"
            case "gako358" => "Gako358"
            case _         => gitConfig
          }

          AuthorizedUser.values
            .find(_.githubUsername.equalsIgnoreCase(username))
            .map(_.githubUsername)
        }
    }).value
