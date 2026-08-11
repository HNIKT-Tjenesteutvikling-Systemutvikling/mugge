package chat

trait UserMapping:
  def mapHostname(hostname: String): String

final class LiveUserMapping private () extends UserMapping:
  override def mapHostname(hostname: String): String =
    hostname match
      case "terangreal"     => "Knut"
      case "tuathaan"       => "Gako"
      case "Solheim"        => "Jan-Magnus"
      case "Turbonaepskrel" => "Magnus"
      case "grindstein"     => "Torkil"
      case "ievensen"       => "Harstad"
      case "intervbs"       => "Joran"
      case "jca"            => "Jan-Olov"
      case "neethan"        => "Neethan"
      case "neethan-hnikt"  => "Neethan"
      case "sigubrat"       => "Sigurd"
      case "williams"       => "Sigurd"
      case "shitbox"        => "Leif"
      case "ketil"          => "Ketil"
      case "work"           => "Kristian"
      case other            => other

object LiveUserMapping:
  def apply(): UserMapping = new LiveUserMapping()
