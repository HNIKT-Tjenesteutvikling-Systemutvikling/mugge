package chat

import cats.effect.*
import cats.syntax.all.*
import java.security.*
import java.security.spec.*
import java.security.interfaces.RSAPrivateCrtKey
import java.util.Base64
import scala.util.Try
import sttp.client3.*
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import java.nio.file.{Files, Paths, LinkOption}
import java.math.BigInteger

object Authentication:

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

  def fetchGithubKeys(username: String): IO[List[PublicKeyInfo]] =
    HttpClientCatsBackend.resource[IO]().use { backend =>
      val request = basicRequest
        .get(uri"https://github.com/$username.keys")
        .response(asString)

      for
        response <- backend.send(request)
        keys <- response.body match
          case Right(body) =>
            IO.pure(parseSSHKeys(body))
          case Left(error) =>
            IO.raiseError(new Exception(s"Failed to fetch keys: $error"))
      yield keys
    }

  private def parseSSHKeys(keysText: String): List[PublicKeyInfo] =
    keysText
      .split("\n")
      .filter(_.trim.nonEmpty)
      .toList
      .flatMap { line =>
        Try {
          val parts = line.trim.split("\\s+")
          if parts.length >= 2 && parts(0) == "ssh-rsa" then
            val keyBytes = Base64.getDecoder.decode(parts(1))
            val keySpec = new X509EncodedKeySpec(convertSSHtoX509(keyBytes))
            val keyFactory = KeyFactory.getInstance("RSA")
            val publicKey = keyFactory.generatePublic(keySpec)
            val fingerprint = calculateFingerprint(keyBytes)
            Some(PublicKeyInfo(publicKey, fingerprint, line))
          else None
        }.toOption.flatten
      }

  private def convertSSHtoX509(sshKey: Array[Byte]): Array[Byte] =
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

    Try {
      val spec = new RSAPublicKeySpec(
        new java.math.BigInteger(1, modulus),
        new java.math.BigInteger(1, exponent)
      )
      val keyFactory = KeyFactory.getInstance("RSA")
      val publicKey = keyFactory.generatePublic(spec)
      publicKey.getEncoded
    }.getOrElse(Array.empty)

  private def calculateFingerprint(keyBytes: Array[Byte]): String =
    val md = MessageDigest.getInstance("SHA-256")
    val hash = md.digest(keyBytes)
    Base64.getEncoder.encodeToString(hash)

  def generateChallenge(): IO[String] =
    IO {
      val random = new SecureRandom()
      val bytes = new Array[Byte](32)
      random.nextBytes(bytes)
      Base64.getEncoder.encodeToString(bytes)
    }

  def loadPrivateKey(
      keyPath: String = s"${System.getProperty("user.home")}/.ssh/id_rsa"
  ): IO[PrivateKey] =
    IO.blocking {
      val home = System.getProperty("user.home")

      val keyPaths = List(
        keyPath, // Default path
        s"$home/.config/sops-nix/secrets/private_keys/gako",
        s"$home/.ssh/id_rsa",
        s"$home/.ssh/id_ed25519",
        s"$home/.ssh/id_ecdsa"
      )

      val existingKeyPath = keyPaths.find { path =>
        val p = Paths.get(path)
        val exists = Files.exists(p)
        if exists then println(s"Found key at: $path")
        exists
      }

      existingKeyPath match {
        case Some(path) =>
          Try {
            val keyBytes = Files.readAllBytes(Paths.get(path))
            val keyString = new String(keyBytes)

            println(s"Reading key from: $path")

            val actualPath = Paths.get(path).toRealPath()
            if actualPath.toString != path then println(s"Key is symlinked to: $actualPath")

            println(s"Key format detected: ${
                if keyString.contains("BEGIN OPENSSH PRIVATE KEY") then "OpenSSH"
                else if keyString.contains("BEGIN RSA PRIVATE KEY") then "PKCS1"
                else if keyString.contains("BEGIN PRIVATE KEY") then "PKCS8"
                else if keyString.contains("BEGIN EC PRIVATE KEY") then "EC"
                else "Unknown"
              }")

            if keyString.contains("BEGIN OPENSSH PRIVATE KEY") then
              convertOpenSSHKey(path, keyString)
            else if keyString.contains("BEGIN RSA PRIVATE KEY") then parsePKCS1Key(keyString)
            else if keyString.contains("BEGIN PRIVATE KEY") then
              val pemContent = keyString
                .replace("-----BEGIN PRIVATE KEY-----", "")
                .replace("-----END PRIVATE KEY-----", "")
                .replaceAll("\\s", "")

              val decoded = Base64.getDecoder.decode(pemContent)
              val keySpec = new PKCS8EncodedKeySpec(decoded)
              val keyFactory = KeyFactory.getInstance("RSA")
              keyFactory.generatePrivate(keySpec)
            else if keyString.contains("BEGIN EC PRIVATE KEY") then
              println("EC keys are not supported. Please use an RSA key.")
              generateTemporaryKey()
            else
              println(s"Unknown key format in $path")
              generateTemporaryKey()
          }.recover { case e =>
            println(s"Failed to load key from $path: ${e.getMessage}")
            generateTemporaryKey()
          }.get

        case None =>
          println(s"No SSH keys found in any of the following locations:")
          keyPaths.foreach(p => println(s"  - $p"))
          generateTemporaryKey()
      }
    }

  private def convertOpenSSHKey(keyPath: String, keyString: String): PrivateKey = {
    Try {
      import scala.sys.process._

      val tempFile = Files.createTempFile("ssh_key", ".pem")
      val tempFileOut = Files.createTempFile("ssh_key_out", ".pem")

      // Write the key to temp file
      Files.write(tempFile, keyString.getBytes)

      // Try to convert using ssh-keygen
      val cmd = Seq("ssh-keygen", "-p", "-m", "PEM", "-f", tempFile.toString, "-P", "", "-N", "")
      val result = cmd.!

      if result == 0 then
        // Read the converted key
        val convertedKey = Files.readString(tempFile)
        Files.delete(tempFile)
        Files.delete(tempFileOut)

        if convertedKey.contains("BEGIN RSA PRIVATE KEY") then parsePKCS1Key(convertedKey)
        else throw new Exception("Conversion did not produce PKCS1 format")
      else
        // If ssh-keygen fails, try using openssl
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
          keyFactory.generatePrivate(keySpec)
        else throw new Exception("Both ssh-keygen conversions failed")
    }.recover { case e =>
      println(s"Failed to convert OpenSSH key: ${e.getMessage}")
      println("Generating temporary key instead...")
      generateTemporaryKey()
    }.get
  }

  private def parsePKCS1Key(keyString: String): PrivateKey = {
    Try {
      // Use openssl to convert PKCS1 to PKCS8
      import scala.sys.process._

      val tempIn = Files.createTempFile("key", ".pem")
      val tempOut = Files.createTempFile("key", ".der")

      Files.write(tempIn, keyString.getBytes)

      // Convert PKCS1 to PKCS8 DER format
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
    }.recover { case e =>
      println(s"Failed to convert PKCS1 key: ${e.getMessage}")
      generateTemporaryKey()
    }.get
  }

  private def generateTemporaryKey(): PrivateKey = {
    println("\nGenerating temporary RSA key for this session...")
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(2048)
    val keyPair = keyGen.generateKeyPair()

    // Save the public key so it can be added to GitHub
    val pubKey = keyPair.getPublic.asInstanceOf[java.security.interfaces.RSAPublicKey]
    val sshPubKey = convertToSSHFormat(pubKey)

    println(s"\nAdd this public key to your GitHub account:")
    println(s"$sshPubKey\n")

    keyPair.getPrivate
  }

  private def convertToSSHFormat(publicKey: java.security.interfaces.RSAPublicKey): String = {
    val typeBytes = "ssh-rsa".getBytes("US-ASCII")
    val exponentBytes = publicKey.getPublicExponent.toByteArray
    val modulusBytes = publicKey.getModulus.toByteArray

    val totalLength = 4 + typeBytes.length + 4 + exponentBytes.length + 4 + modulusBytes.length
    val buffer = java.nio.ByteBuffer.allocate(totalLength)

    buffer.putInt(typeBytes.length)
    buffer.put(typeBytes)
    buffer.putInt(exponentBytes.length)
    buffer.put(exponentBytes)
    buffer.putInt(modulusBytes.length)
    buffer.put(modulusBytes)

    s"ssh-rsa ${Base64.getEncoder.encodeToString(buffer.array())} terminal-chat-temp"
  }

  def getPublicKeyFromPrivate(privateKey: PrivateKey): PublicKey =
    privateKey match
      case rsaPrivate: RSAPrivateCrtKey =>
        val spec = new RSAPublicKeySpec(rsaPrivate.getModulus, rsaPrivate.getPublicExponent)
        val keyFactory = KeyFactory.getInstance("RSA")
        keyFactory.generatePublic(spec)
      case _ =>
        throw new Exception("Unsupported private key type")

  def getLocalSSHPublicKey(
      keyPath: String = s"${System.getProperty("user.home")}/.ssh/id_rsa.pub"
  ): IO[String] =
    IO.blocking {
      Files.readString(Paths.get(keyPath)).trim
    }

  def signChallenge(challenge: String, privateKey: PrivateKey): IO[String] =
    IO {
      val signature = Signature.getInstance("SHA256withRSA")
      signature.initSign(privateKey)
      signature.update(challenge.getBytes("UTF-8"))
      val signatureBytes = signature.sign()
      Base64.getEncoder.encodeToString(signatureBytes)
    }

  def verifySignature(challenge: String, signature: String, publicKey: PublicKey): IO[Boolean] =
    IO {
      Try {
        val sig = Signature.getInstance("SHA256withRSA")
        sig.initVerify(publicKey)
        sig.update(challenge.getBytes("UTF-8"))
        sig.verify(Base64.getDecoder.decode(signature))
      }.getOrElse(false)
    }

  def detectGithubUsername(): IO[Option[String]] =
    IO.blocking {
      val gitConfig = Try {
        scala.sys.process.Process(Seq("git", "config", "--global", "user.name")).!!.trim
      }.getOrElse("")

      val username = gitConfig.toLowerCase match {
        case "merrinx" => "Gako358"
        case "gako358" => "Gako358"
        case _         => gitConfig
      }

      AuthorizedUser.values
        .find(_.githubUsername.equalsIgnoreCase(username))
        .map(_.githubUsername)
    }
