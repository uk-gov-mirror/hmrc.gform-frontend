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

package uk.gov.hmrc.gform

import _root_.akka.stream.Materializer
import play.api.mvc.{ Cookie, Filter, RequestHeader, Result }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, FormTemplate, FormTemplateId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoFilter
import play.api.routing.Router.RequestImplicits._

class MyFilter(
  cookieCryptoFilter: SessionCookieCryptoFilter,
  gformCookieCryptoFilter: SessionCookieCryptoFilter,
  gformConnector: GformConnector
)(implicit ec: ExecutionContext, override val mat: Materializer)
    extends Filter {

  val gformCookieName = "submissions"

  def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {

    val read = rh.cookies.get(gformCookieName).fold("")(_.value)

    val requestWithFormTemplateId = rh.handlerDef.exists { hd =>
      hd.parameterTypes.exists { cls =>
        cls.getName() == "uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId"
      }
    }

    val maybeFormTemplate: Future[Either[Unit, FormTemplate]] =
      if (requestWithFormTemplateId) {
        val templateId = rh.uri.split("/")(3)
        //println("templateId: " + templateId)
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(rh.headers, Some(rh.session))
        gformConnector.getFormTemplate(FormTemplateId(templateId)).map(Right(_))
      } else {
        Future.successful(Left(()))
      }

    maybeFormTemplate.flatMap {
      case Right(formTemplate) =>
        val (result, cookieValue) =
          formTemplate.authConfig match {
            case Anonymous =>
              (gformCookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplate)), "anon")
            case _ =>
              (cookieCryptoFilter(next)(rh.addAttr(FormTemplateKey, formTemplate)), "hmrc")
          }

        result.map(_.withCookies(Cookie(gformCookieName, cookieValue)))
      case Left(_) =>
        if (read == "anon") {
          gformCookieCryptoFilter(next)(rh)
        } else {
          cookieCryptoFilter(next)(rh)
        }
    }
  }
}
