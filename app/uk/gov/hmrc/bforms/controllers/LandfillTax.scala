/*
 * Copyright 2016 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.controllers

import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.mvc._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

object LandfillTax extends LandfillTax

trait LandfillTax extends FrontendController {
  def landfillTaxDisplay (registrationNumber : String) = Action.async { implicit request =>
		Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax(registrationNumber.filter(Character.isLetterOrDigit))))
  }
  def landfillTaxSubmitContinue(registrationNumber : String) = Action.async { Future.successful(Redirect(routes.LandfillTaxForm.landfillTaxFormDisplay(registrationNumber))) }
}
