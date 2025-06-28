package chat

import cats.effect.*
import cats.syntax.all.*
import cats.data.EitherT
import java.security.*
import java.security.spec.*
import java.security.interfaces.RSAPrivateCrtKey
import java.util.Base64
import sttp.client3.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import java.nio.file.{Files, Paths, LinkOption}
import java.math.BigInteger
import scala.sys.process.*

object Authentication:

  sealed trait AuthError extends Exception:
    def message: String
    override def getMessage: String = message

  case class KeyFetchError(message: String) extends AuthError
  case class KeyParseError(message: String) extends AuthError
  case class KeyLoadError(message: String) extends AuthError
  case class KeyConversionError(message: String) extends AuthError
  case class SignatureError(message: String) extends AuthError
  case class GitConfigError(message: String) extends AuthError

  enum AuthorizedUser(val githubUsername: String):
    case Gako358 extends AuthorizedUser("Gako358")
    case Merrinx extends AuthorizedUser("merrinx")

  case class AuthChallenge(challenge: String, signature: Option[String])

  case class PublicKeyInfo(
      key: PublicKey,
      fingerprint: String,
      rawKey: String
  )

  // Cache timeout: 24 hours (in milliseconds)
  val CACHE_TIMEOUT_MS: Long = 24 * 60 * 60 * 1000L

  def fetchGithubKeys(username: String): IO[Either[AuthError, List[PublicKeyInfo]]] =
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val request = basicRequest
        .get(uri"https://github.com/$username.keys")
        .response(asString)

      (for
        response <- EitherT
          .liftF[IO, AuthError, Response[Either[String, String]]](backend.send(request))
        body <- EitherT.fromEither[IO](
          response.body.leftMap(error => KeyFetchError(s"Failed to fetch keys from GitHub: $error"))
        )
        keys <- parseSSHKeys(body)
      yield keys).value
    }

  private def parseSSHKeys(keysText: String): EitherT[IO, AuthError, List[PublicKeyInfo]] =
    EitherT.fromEither[IO] {
      val lines = keysText
        .split("\n")
        .filter(_.trim.nonEmpty)
        .toList

      lines.traverse { line =>
        val parts = line.trim.split("\\s+")
        if parts.length >= 2 && parts(0) == "ssh-rsa" then
          for
            keyBytes <- Either
              .catchNonFatal(Base64.getDecoder.decode(parts(1)))
              .leftMap(e => KeyParseError(s"Failed to decode base64 for key: ${e.getMessage}"))
            x509Bytes <- convertSSHtoX509(keyBytes)
            keySpec = new X509EncodedKeySpec(x509Bytes)
            keyFactory <- Either
              .catchNonFatal(KeyFactory.getInstance("RSA"))
              .leftMap(e => KeyParseError(s"Failed to get RSA key factory: ${e.getMessage}"))
            publicKey <- Either
              .catchNonFatal(keyFactory.generatePublic(keySpec))
              .leftMap(e => KeyParseError(s"Failed to generate public key: ${e.getMessage}"))
            fingerprint <- calculateFingerprint(keyBytes)
          yield PublicKeyInfo(publicKey, fingerprint, line)
        else
          Left(
            KeyParseError(
              s"Invalid SSH key format: expected ssh-rsa, got ${parts.headOption.getOrElse("empty")}"
            )
          )
      }
    }

  private def convertSSHtoX509(sshKey: Array[Byte]): Either[AuthError, Array[Byte]] =
    Either
      .catchNonFatal {
        var offset = 0

        val typeLen = ((sshKey(offset) & 0xff) << 24) |
          ((sshKey(offset + 1) & 0xff) << 16) |
          ((sshKey(offset + 2) & 0xff) << 8) |
          (sshKey(offset + 3) & 0xff)
        offset += 4 + typeLen

        val expLen = ((sshKey(offset) & 0xff) << 24) |
          ((sshKey(offset + 1) & 0xff) << 16) |
          ((sshKey(offset + 2) & 0xff) << 8) |
          (sshKey(offset + 3) & 0xff)
        offset += 4
        val exponent = sshKey.slice(offset, offset + expLen)
        offset += expLen

        val modLen = ((sshKey(offset) & 0xff) << 24) |
          ((sshKey(offset + 1) & 0xff) << 16) |
          ((sshKey(offset + 2) & 0xff) << 8) |
          (sshKey(offset + 3) & 0xff)
        offset += 4
        val modulus = sshKey.slice(offset, offset + modLen)

        val spec = new RSAPublicKeySpec(
          new java.math.BigInteger(1, modulus),
          new java.math.BigInteger(1, exponent)
        )
        val keyFactory = KeyFactory.getInstance("RSA")
        val publicKey = keyFactory.generatePublic(spec)
        publicKey.getEncoded
      }
      .leftMap(e => KeyConversionError(s"Failed to convert SSH key to X509: ${e.getMessage}"))

  private def calculateFingerprint(keyBytes: Array[Byte]): Either[AuthError, String] =
    Either
      .catchNonFatal {
        val md = MessageDigest.getInstance("SHA-256")
        val hash = md.digest(keyBytes)
        Base64.getEncoder.encodeToString(hash)
      }
      .leftMap(e => KeyParseError(s"Failed to calculate fingerprint: ${e.getMessage}"))

  def generateChallenge(): IO[Either[AuthError, String]] =
    EitherT
      .fromEither[IO] {
        Either
          .catchNonFatal {
            val random = new SecureRandom()
            val bytes = new Array[Byte](32)
            random.nextBytes(bytes)
            Base64.getEncoder.encodeToString(bytes)
          }
          .leftMap(e => SignatureError(s"Failed to generate challenge: ${e.getMessage}"))
      }
      .value

  def loadPrivateKey(
      keyPath: String = s"${System.getProperty("user.home")}/.ssh/id_rsa"
  ): IO[Either[AuthError, PrivateKey]] =
    (for
      home <- EitherT.pure[IO, AuthError](System.getProperty("user.home"))
      keyPaths = List(
        keyPath,
        s"$home/.config/sops-nix/secrets/private_keys/gako",
        s"$home/.ssh/id_rsa",
        s"$home/.ssh/id_ed25519",
        s"$home/.ssh/id_ecdsa"
      )
      existingKeyPath <- EitherT.fromEither[IO] {
        keyPaths
          .find { path =>
            val p = Paths.get(path)
            val exists = Files.exists(p)
            if exists then println(s"Found key at: $path")
            exists
          }
          .toRight(
            KeyLoadError(
              s"No SSH keys found in any of the following locations: ${keyPaths.mkString(", ")}"
            )
          )
      }
      keyBytes <- EitherT(IO.blocking {
        Either
          .catchNonFatal(Files.readAllBytes(Paths.get(existingKeyPath)))
          .leftMap(e => KeyLoadError(s"Failed to read key file: ${e.getMessage}"))
      })
      keyString = new String(keyBytes)
      _ = println(s"Reading key from: $existingKeyPath")
      actualPath <- EitherT(IO.blocking {
        Either
          .catchNonFatal(Paths.get(existingKeyPath).toRealPath())
          .leftMap(e => KeyLoadError(s"Failed to resolve key path: ${e.getMessage}"))
      })
      _ = if actualPath.toString != existingKeyPath then
        println(s"Key is symlinked to: $actualPath")
      _ = println(s"Key format detected: ${detectKeyFormat(keyString)}")
      privateKey <- parsePrivateKey(existingKeyPath, keyString)
    yield privateKey).value

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

  def getPublicKeyFromPrivate(privateKey: PrivateKey): Either[AuthError, PublicKey] =
    privateKey match
      case rsaPrivate: RSAPrivateCrtKey =>
        Either
          .catchNonFatal {
            val spec = new RSAPublicKeySpec(rsaPrivate.getModulus, rsaPrivate.getPublicExponent)
            val keyFactory = KeyFactory.getInstance("RSA")
            keyFactory.generatePublic(spec)
          }
          .leftMap(e => KeyConversionError(s"Failed to derive public key: ${e.getMessage}"))
      case _ =>
        Left(KeyConversionError("Unsupported private key type"))

  def getLocalSSHPublicKey(
      keyPath: String = s"${System.getProperty("user.home")}/.ssh/id_rsa.pub"
  ): IO[Either[AuthError, String]] =
    EitherT(IO.blocking {
      Either
        .catchNonFatal(Files.readString(Paths.get(keyPath)).trim)
        .leftMap(e => KeyLoadError(s"Failed to read public key file: ${e.getMessage}"))
    }).value

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

  def verifySignature(
      challenge: String,
      signature: String,
      publicKey: PublicKey
  ): IO[Either[AuthError, Boolean]] =
    EitherT
      .fromEither[IO] {
        Either
          .catchNonFatal {
            val sig = Signature.getInstance("SHA256withRSA")
            sig.initVerify(publicKey)
            sig.update(challenge.getBytes("UTF-8"))
            sig.verify(Base64.getDecoder.decode(signature))
          }
          .leftMap(e => SignatureError(s"Failed to verify signature: ${e.getMessage}"))
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
