package chat

import cats.effect.*
import fs2.io.net.Network
import fs2.io.net.tls.TLSContext
import fs2.io.net.tls.TLSParameters

import java.security.MessageDigest
import java.security.SecureRandom
import java.security.cert.CertificateException
import java.security.cert.X509Certificate
import java.util.Base64
import javax.net.ssl.SSLContext
import javax.net.ssl.X509TrustManager

object Tls:
  // Base64 SHA-256 of the server's SubjectPublicKeyInfo, pinned like the
  // AuthorizedUser allowlist (public by design). Primary is deployed via the
  // server's MUGGE_TLS_KEY; backup is the offline spare kept for rotation.
  // Mint/rotate with `nix develop -c mintTlsKey <name>` in mugge-server.
  private val pins: Set[String] = Set(
    "ddH9tVk8fuIHLAkKZo/rQez35V24SrQ2pe1Bd4jhvq0=",
    "xabpD6pP6GsRQ8kaM4D+2aDZVbWSTq7V4G5kwlZnRzs="
  )

  val parameters: TLSParameters =
    TLSParameters(
      protocols = Some(List("TLSv1.3")),
      cipherSuites = Some(List("TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256"))
    )

  final class PinMismatch(val fingerprint: String)
      extends Exception(
        "Server key does not match the pinned key — possible MITM or key " +
          "rotation; update the client."
      )

  private def fingerprint(cert: X509Certificate): String =
    Base64.getEncoder.encodeToString(
      MessageDigest.getInstance("SHA-256").digest(cert.getPublicKey.getEncoded)
    )

  private val pinningTrustManager: X509TrustManager =
    new X509TrustManager:
      def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = ()
      def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit =
        val fp = fingerprint(chain(0))
        if !pins.contains(fp) then
          throw new CertificateException(new PinMismatch(fp)) // scalafix:ok DisableSyntax.throw
      def getAcceptedIssuers: Array[X509Certificate] = Array.empty

  def pinnedContext: IO[TLSContext[IO]] =
    IO.blocking {
      val ctx = SSLContext.getInstance("TLS")
      ctx.init(
        null, // scalafix:ok DisableSyntax.null
        Array(pinningTrustManager),
        new SecureRandom()
      )
      ctx
    }.map(Network[IO].tlsContext.fromSSLContext)

  def isPinMismatch(err: Throwable): Boolean =
    Option(err) match
      case Some(_: PinMismatch) => true
      case Some(e)              => isPinMismatch(e.getCause)
      case None                 => false
