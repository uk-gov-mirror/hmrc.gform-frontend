/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

sealed trait Validator {
  def errorMessage: String
}

case object Validator {
  private val basic: OFormat[Validator] = derived.oformat

  private val templateReads: Reads[Validator] = Reads { json =>
    (json \ "validatorName").as[String] match {
      case "hmrcUTRPostcodeCheck"    => json.validate[HMRCUTRPostcodeCheckValidator]
      case "bankAccountModulusCheck" => json.validate[BankAccoutnModulusCheck]
      case unsupported               => JsError("Unsupported '" + unsupported + "' kind of validator.")
    }
  }

  private val reads = (basic: Reads[Validator]) | templateReads

  implicit val format: OFormat[Validator] = OFormat(reads, basic)
}

case class HMRCUTRPostcodeCheckValidator(errorMessage: String, utr: FormCtx, postcode: FormCtx) extends Validator {

  val utrFieldId = FormComponentId(utr.value)
  val postcodeFieldId = FormComponentId(postcode.value)
}

object HMRCUTRPostcodeCheckValidator {
  private val readCustom: Reads[HMRCUTRPostcodeCheckValidator] =
    ((JsPath \ "errorMessage").read[String] and
      (JsPath \ "parameters" \ "utr").read(FormCtx.readsForTemplateJson) and
      (JsPath \ "parameters" \ "postcode").read(FormCtx.readsForTemplateJson))(HMRCUTRPostcodeCheckValidator.apply _)

  implicit val format: OFormat[HMRCUTRPostcodeCheckValidator] = OFormatWithTemplateReadFallback(readCustom)
}

case class BankAccoutnModulusCheck(errorMessage: String, accountNumber: FormCtx, sortCode: FormCtx) extends Validator {

  val accountNumberId = accountNumber.toFieldId
  val sortCodeId = sortCode.toFieldId
}

object BankAccoutnModulusCheck {
  private val readCustom: Reads[BankAccoutnModulusCheck] =
    ((JsPath \ "errorMessage").read[String] and
      (JsPath \ "parameters" \ "accountNumber").read(FormCtx.readsForTemplateJson) and
      (JsPath \ "parameters" \ "sortCode").read(FormCtx.readsForTemplateJson))(BankAccoutnModulusCheck.apply _)

  implicit val format: OFormat[BankAccoutnModulusCheck] = OFormatWithTemplateReadFallback(readCustom)
}
