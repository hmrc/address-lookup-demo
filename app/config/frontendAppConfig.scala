package config

import play.api.Play.{configuration, current}
import uk.gov.hmrc.play.config.ServicesConfig

trait AppConfig {
  val assetsPrefix: String
  val analyticsToken: String
  val analyticsHost: String
  val reportAProblemPartialUrl: String
  val reportAProblemNonJSUrl: String
}

object FrontendAppConfig extends AppConfig with ServicesConfig {

  private def loadConfig(key: String) = configuration.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  private val contactHost = configuration.getString(s"$env.contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = "MyService"

  override lazy val assetsPrefix = loadConfig(s"$env.assets.url") + loadConfig(s"$env.assets.version")
  override lazy val analyticsToken = loadConfig(s"$env.google-analytics.token")
  override lazy val analyticsHost = loadConfig(s"$env.google-analytics.host")
  override lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  override lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"
}
