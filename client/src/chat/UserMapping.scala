package chat

trait UserMapping:
  def mapHostname(hostname: String): String

object UserMapping:
  private[chat] val hostnameToName: Map[String, String] = Map(
    "terangreal" -> "Knut",
    "tuathaan" -> "Gako",
    "hnikt-tos-40067" -> "Vebjorn",
    "solheim" -> "Jan-Magnus",
    "turbonaepskrel" -> "Magnus",
    "grindstein" -> "Torkil",
    "ievensen" -> "Harstad",
    "intervbs" -> "Joran",
    "jca" -> "Jan-Olov",
    "neethan" -> "Neethan",
    "neethan-hnikt" -> "Neethan",
    "sigubrat" -> "Sigurd",
    "williams" -> "Sigurd",
    "shitbox" -> "Leif",
    "ketil" -> "Ketil",
    "work" -> "Kristian"
  )

final class LiveUserMapping private () extends UserMapping:
  override def mapHostname(hostname: String): String =
    UserMapping.hostnameToName.getOrElse(hostname.toLowerCase, hostname)

object LiveUserMapping:
  def apply(): UserMapping = new LiveUserMapping()
