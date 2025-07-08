package chat

object UserMapping:
  def mapHostname(hostname: String): String =
    hostname match
      case "terangreal"     => "Knut"
      case "tuathaan"       => "Gako"
      case "Solheim"        => "Jan-Magnus"
      case "Turbonaepskrel" => "Magnus"
      case "grindstein"     => "Torkil"
      case "ievensen"       => "Harstad"
      case "intervbs"       => "Joran"
      case "jca"            => "Svensken"
      case "neethan"        => "Neethan"
      case "sigubrat"       => "Sigurd"
      case other            => other
