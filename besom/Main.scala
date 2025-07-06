import besom.*
import besom.api.azurenative
import besom.api.azurenative.web.inputs.NameValuePairArgs
import besom.internal.Context
import cats.syntax.all.*

def configToNameValue(
    configName: NonEmptyString
)(using Context): Output[Option[NameValuePairArgs]] =
  Pulumi.config
    .getString(configName)
    .map(_.map(s => NameValuePairArgs(configName.split(":").last, s)))

def azureConfig(using Context): Output[Option[List[NameValuePairArgs]]] =
  for
    dockerPw <- configToNameValue(
      "mugge-chat:DOCKER_REGISTRY_SERVER_PASSWORD"
    )
    dockerUrl <- configToNameValue(
      "mugge-chat:DOCKER_REGISTRY_SERVER_URL"
    )
    dockerUser <- configToNameValue(
      "mugge-chat:DOCKER_REGISTRY_SERVER_USERNAME"
    )
  yield List(
    dockerPw,
    dockerUrl,
    dockerUser
  ).sequence

val tags = Map(
  "owner" -> "kvalitetsregister",
  "environment" -> "test",
  "businessCriticalitw" -> "TN3",
  "businessUnit" -> "Applikasjonstjenester",
  "costCenter" -> "0",
  "dataClassification" -> "Confidential",
  "workloadName" -> "",
  "test" -> "test"
)

@main def main = Pulumi.run {

  val resourceArgs =
    azurenative.resources.ResourceGroupArgs(
      location = "NorwayEast",
      tags = tags
    )

  val resourceGroupName = azurenative.resources
    .ResourceGroup("mugge-chat", resourceArgs)
    .name

  def image(hash: Option[String]) =
    s"ghcr.io/hnikt-tjenesteutvikling-systemutvikling/mugge:${hash.getOrElse("latest")}"

  val plan = azurenative.web.AppServicePlan(
    "mugge-chat-plan",
    azurenative.web.AppServicePlanArgs(
      resourceGroupName = resourceGroupName,
      kind = "linux",
      reserved = true,
      sku = Some(
        azurenative.web.inputs.SkuDescriptionArgs(name = "B3", tier = "Basic")
      ),
      location = "NorwayEast",
      tags = tags
    )
  )

  def siteConfigArgs(
      extraArgs: Option[List[NameValuePairArgs]],
      hash: Option[String]
  ) =
    azurenative.web.inputs.SiteConfigArgs(
      appSettings = extraArgs.map(
        _ :+
          NameValuePairArgs("WEBSITES_ENABLE_APP_SERVICE_STORAGE", "true") :+
          NameValuePairArgs("WEBSITES_CONTAINER_START_TIME_LIMIT", "1800") :+
          NameValuePairArgs("RUNNING_ENV", "test") :+
          NameValuePairArgs("WEBSITES_PORT", "8080") :+
          NameValuePairArgs("SECURITY_CLIENT_ID", "mugge-test-azure") :+
          NameValuePairArgs("SECURITY_APPLICATION_SCOPE", "mugge-test-azure") :+
          NameValuePairArgs(
            "SECURITY_REDIRECT_URL",
            "https://mugge.azurewebsites.net/api/callback/signin-oidc"
          ) :+
          NameValuePairArgs("SECURITY_AUTHENTICATED_URL", "https://mugge.azurewebsites.net")
      ),
      alwaysOn = true,
      linuxFxVersion = s"DOCKER|${image(hash)}"
    )

  def webAppArgs(
      extraArgs: Option[List[NameValuePairArgs]],
      hash: Option[String]
  ) =
    azurenative.web.WebAppArgs(
      name = "mugge",
      resourceGroupName = resourceGroupName,
      serverFarmId = plan.id,
      siteConfig = siteConfigArgs(extraArgs, hash),
      httpsOnly = true,
      tags = tags
    )

  def webApp(extraArgs: Option[List[NameValuePairArgs]], hash: Option[String]) =
    azurenative.web.WebApp("mugge", webAppArgs(extraArgs, hash))

  Stack(for
    azureConf <- azureConfig
    hash <- config.getString("hash")
    webapp <- webApp(azureConf, hash)
  yield webapp).exports()
}
