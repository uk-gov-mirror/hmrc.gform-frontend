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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import play.api.mvc.{ EssentialFilter, SessionCookieBaker }
import play.filters.csrf.CSRFComponents
import play.filters.headers.SecurityHeadersFilter
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.play.bootstrap.config.DefaultHttpAuditEvent
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.DefaultSessionCookieCryptoFilter
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoFilter
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoProvider
import uk.gov.hmrc.play.bootstrap.frontend.filters.deviceid.DefaultDeviceIdFilter
import uk.gov.hmrc.play.bootstrap.frontend.filters.{ AllowlistFilter, DefaultFrontendAuditFilter, FrontendFilters, HeadersFilter, SessionIdFilter, SessionTimeoutFilter, SessionTimeoutFilterConfig }
import uk.gov.hmrc.play.bootstrap.filters.{ CacheControlConfig, CacheControlFilter, DefaultLoggingFilter, MDCFilter }

import scala.concurrent.ExecutionContext

import uk.gov.hmrc.crypto._

class AnonoymousSessionCookieCryptoFilter(
  sessionCookieCrypto: SessionCookieCrypto,
  val sessionBaker: SessionCookieBaker
)(implicit override val mat: Materializer, override val ec: ExecutionContext)
    extends SessionCookieCryptoFilter {
  override protected lazy val encrypter: Encrypter = sessionCookieCrypto.crypto
  override protected lazy val decrypter: Decrypter = sessionCookieCrypto.crypto
}

class FrontendFiltersModule(
  gformBackendModule: GformBackendModule,
  applicationCrypto: ApplicationCrypto,
  playBuiltInsModule: PlayBuiltInsModule,
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  metricsModule: MetricsModule,
  controllersModule: ControllersModule,
  csrfComponents: CSRFComponents,
  anonoymousSessionCookieBaker: SessionCookieBaker,
  hmrcSessionCookieBaker: SessionCookieBaker
)(implicit ec: ExecutionContext) { self =>
  private implicit val materializer: Materializer = akkaModule.materializer

  private val frontendAuditFilter = new DefaultFrontendAuditFilter(
    configModule.controllerConfigs,
    auditingModule.auditConnector,
    new DefaultHttpAuditEvent(configModule.appConfig.appName),
    materializer
  ) {
    override val maskedFormFields = Seq("password")
  }

  private val hmrcSessionCookieCryptoFilter: SessionCookieCryptoFilter = {
    val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)
    val sessionCookieCrypto: SessionCookieCrypto = new SessionCookieCryptoProvider(applicationCrypto).get()

    new DefaultSessionCookieCryptoFilter(sessionCookieCrypto, hmrcSessionCookieBaker)
  }

  private val anonoymousSessionCookieCryptoFilter: SessionCookieCryptoFilter = {
    val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)
    val sessionCookieCrypto: SessionCookieCrypto = new SessionCookieCryptoProvider(applicationCrypto).get()

    new AnonoymousSessionCookieCryptoFilter(sessionCookieCrypto, anonoymousSessionCookieBaker)
  }

  private val cacheControlFilter: CacheControlFilter = {
    val cacheControlConfig: CacheControlConfig = CacheControlConfig.fromConfig(configModule.playConfiguration)
    new CacheControlFilter(cacheControlConfig, materializer)
  }

  private val mdcFilter: MDCFilter =
    new MDCFilter(materializer, configModule.playConfiguration, configModule.appConfig.appName)

  private val sessionTimeoutFilter = new SessionTimeoutFilter(
    SessionTimeoutFilterConfig.fromConfig(configModule.playConfiguration)
  )

  private val deviceIdFilter = new DefaultDeviceIdFilter(
    configModule.appConfig.appName,
    configModule.playConfiguration,
    auditingModule.auditConnector
  )

  private val securityHeadersFilter = SecurityHeadersFilter(configModule.playConfiguration)

  private val headersFilter = new HeadersFilter(materializer)

  private val loggingFilter = new DefaultLoggingFilter(configModule.controllerConfigs)

  private val allowListFilter = new AllowlistFilter(configModule.playConfiguration, materializer)

  private val sessionIdFilter = new SessionIdFilter(materializer, ec, hmrcSessionCookieBaker)

  private val sessionCookieDispatcherFilter = {
    val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)
    val sessionCookieCrypto: SessionCookieCrypto = new SessionCookieCryptoProvider(applicationCrypto).get()

    new SessionCookieDispatcherFilter(
      sessionCookieCrypto,
      hmrcSessionCookieCryptoFilter,
      anonoymousSessionCookieCryptoFilter,
      gformBackendModule.gformConnector
    )
  }

  lazy val httpFilters: Seq[EssentialFilter] = new FrontendFilters(
    configModule.playConfiguration,
    loggingFilter,
    headersFilter,
    securityHeadersFilter,
    frontendAuditFilter,
    metricsModule.metricsFilter,
    deviceIdFilter,
    csrfComponents.csrfFilter,
    hmrcSessionCookieCryptoFilter,
    sessionTimeoutFilter,
    cacheControlFilter,
    mdcFilter,
    allowListFilter,
    sessionIdFilter
  ).filters map {
    case _: SessionCookieCryptoFilter => sessionCookieDispatcherFilter
    case other                        => other
  }
}
