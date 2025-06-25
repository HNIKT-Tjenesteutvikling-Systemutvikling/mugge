package chat

object UserMapping:
  def mapHostname(hostname: String): String =
    hostname match
      case "terangreal"  => "Knut"
      case "gako-laptop" => "Gako358"
      case "server-01"   => "ServerAdmin"
      case other         => other
