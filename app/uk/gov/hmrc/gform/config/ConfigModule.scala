/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.config

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import play.api.libs.ws.WSClient
import play.api.{ ApplicationLoader, Configuration, Environment }
import play.api.Mode
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.hmrcfrontend.config.TrackingConsentConfig
import uk.gov.hmrc.hmrcfrontend.views.html.helpers.hmrcTrackingConsentSnippet
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.bootstrap.config.{ AuditingConfigProvider, ControllerConfig, ControllerConfigs, ServicesConfig }

class ConfigModule(val context: ApplicationLoader.Context, playBuiltInsModule: PlayBuiltInsModule, wsClient: WSClient) {

  val playConfiguration: Configuration = context.initialConfiguration
  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()
  val environment: Environment = context.environment

  val mode: Mode = environment.mode

  val timeOut: Int = typesafeConfig.getInt("future.timeout")

  val appConfig: AppConfig = AppConfig.loadOrThrow()

  val serviceConfig = new ServicesConfig(playConfiguration)

  val controllerConfigs = ControllerConfigs.fromConfig(playConfiguration)

  val controllerConfig: ControllerConfig = new ControllerConfig {
    //val controllerConfigs: TypeSafeConfig = typesafeConfig.as[TypeSafeConfig]("controllers")
  }

  val auditingConfig: AuditingConfig = new AuditingConfigProvider(playConfiguration, appConfig.appName).get()

  val availableLanguages: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))
  def routeToSwitchLanguage: String => Call =
    (lang: String) => uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguage(lang)

  val frontendAppConfig: FrontendAppConfig = {
    def getJSConfig(path: String) =
      JSConfig(
        typesafeConfig.getBoolean(s"$path.timeoutEnabled"),
        typesafeConfig.getInt(s"$path.timeout"),
        typesafeConfig.getInt(s"$path.countdown"),
        typesafeConfig.getString(s"$path.keepAliveUrl"),
        typesafeConfig.getString(s"$path.signOutUrl")
      )
    val contactFormServiceIdentifier = "GForm"
    FrontendAppConfig(
      albAdminIssuerUrl =
        playConfiguration.getOptional[String]("albAdminIssuerUrl").getOrElse("idp-url-variable-not-set"),
      reportAProblemPartialUrl = s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier",
      reportAProblemNonJSUrl = s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier",
      governmentGatewaySignInUrl = typesafeConfig.getString("government-gateway-sign-in-url"),
      gformFrontendBaseUrl = typesafeConfig.getString("gform-frontend-base-url"),
      signOutUrl = typesafeConfig.getString("signout-url"),
      footerCookiesUrl = typesafeConfig.getString("footer-cookies-url"),
      footerPrivacyPolicyUrl = typesafeConfig.getString("footer-privacy-policy-url"),
      footerTermsConditionsUrl = typesafeConfig.getString("footer-terms-conditions-url"),
      footerHelpUrl = typesafeConfig.getString("footer-help-url"),
      footerAccessibilityStatementUrl = typesafeConfig.getString("footer-accessibility-statement-url"),
      betaFeedbackUrlNoAuth = s"/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier",
      authModule = AuthModule(
        getJSConfig("auth-module.hmrc"),
        getJSConfig("auth-module.anonymous"),
        getJSConfig("auth-module.awsAlbAuth"),
        getJSConfig("auth-module.email")
      ),
      availableLanguages = availableLanguages,
      routeToSwitchLanguage = routeToSwitchLanguage,
      contactFormServiceIdentifier = contactFormServiceIdentifier,
      optimizelyUrl = for {
        url       <- playConfiguration.getOptional[String]("optimizely.url")
        projectId <- playConfiguration.getOptional[String]("optimizely.projectId")
      } yield s"$url$projectId.js",
      trackingConsentSnippet = new hmrcTrackingConsentSnippet(new TrackingConsentConfig(playConfiguration))
    )
  }
}
