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

package uk.gov.hmrc.gform.gform

import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.auth.models.EmailAuthDetails
import uk.gov.hmrc.gform.auth.models.OperationWithForm.ViewSaveAcknowledgement
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.GformSessionKeys.EMAIL_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailAuthConfig, FormTemplateId }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement_email_auth
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import cats.implicits._
import uk.gov.hmrc.gform.models.SectionSelectorType

import scala.concurrent.{ ExecutionContext, Future }

class SaveAcknowledgementController(
  i18nSupport: I18nSupport,
  frontendAppConfig: FrontendAppConfig,
  auth: AuthenticatedRequestActions,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def show(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, ViewSaveAcknowledgement) {
      implicit request => implicit lang => cache => _ => _ =>
        val formTemplate = cache.formTemplate
        formTemplate.authConfig match {
          case EmailAuthConfig(_) =>
            val emailAuthDetails: EmailAuthDetails =
              jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
            emailAuthDetails
              .get(formTemplateId)
              .fold {
                throw new IllegalArgumentException(
                  s"Email auth details missing for form template $formTemplateId"
                )
              } { emailAuthDetails =>
                Ok(
                  save_acknowledgement_email_auth(
                    formTemplate,
                    uk.gov.hmrc.gform.models.EmailId(emailAuthDetails.email),
                    frontendAppConfig
                  )
                )
              }
              .pure[Future]
          case _ =>
            throw new IllegalArgumentException(s"Only forms with 'email' auth config allowed")
        }
    }
}
