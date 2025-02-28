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

package uk.gov.hmrc.gform.models

import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation._

class FormFieldValidationResultSpec extends FlatSpec with Matchers {

  val mockDateFieldValue = FormComponent(
    id = FormComponentId("StartDate"),
    `type` = Date(AnyDate, Offset(0), Some(ExactDateValue(2010, 10, 10))),
    label = toSmartString("label"),
    helpText = None,
    shortName = None,
    mandatory = false,
    includeIf = None,
    validIf = None,
    editable = true,
    submissible = true,
    derived = true,
    onlyShowOnSummary = false,
    errorMessage = None
  )

  def mkHtmlFieldId(value: String): HtmlFieldId =
    HtmlFieldId.pure(
      ModelComponentId.atomic(IndexedComponentId.pure(BaseComponentId(mockDateFieldValue.id.value)), Atom(value))
    )

  "getOptionalCurrentValue" should "return current value when validation returns FieldError" in {
    val testFieldComponent = ComponentField(
      formComponent = mockDateFieldValue,
      data = Map(
        mkHtmlFieldId("day") -> FieldOk(
          FormComponent(
            FormComponentId("day"),
            Text(TextConstraint.default, Value),
            toSmartString("label"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "1"
        ),
        mkHtmlFieldId("month") -> FieldOk(
          FormComponent(
            FormComponentId("month"),
            Text(TextConstraint.default, Value),
            toSmartString("label"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "1"
        ),
        mkHtmlFieldId("year") -> FieldError(
          FormComponent(
            FormComponentId("year"),
            Text(TextConstraint.default, Value),
            toSmartString("label"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "NOT_RIGHT",
          Set("NAN")
        )
      )
    )

    testFieldComponent.getOptionalCurrentValue(mkHtmlFieldId("year")).get.equalsIgnoreCase("NOT_RIGHT") shouldBe true

  }

  it should "return current value when validation returns other than FieldError" in {
    val testFieldComponent = ComponentField(
      formComponent = mockDateFieldValue,
      data = Map(
        mkHtmlFieldId("day") -> FieldOk(
          FormComponent(
            FormComponentId("day"),
            Text(TextConstraint.default, Value),
            toSmartString("day"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "1"
        ),
        mkHtmlFieldId("month") -> FieldOk(
          FormComponent(
            FormComponentId("month"),
            Text(TextConstraint.default, Value),
            toSmartString("month"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "1"
        ),
        mkHtmlFieldId("year") -> FieldOk(
          FormComponent(
            FormComponentId("year"),
            Text(TextConstraint.default, Value),
            toSmartString("year"),
            None,
            None,
            None,
            None,
            false,
            true,
            true,
            true,
            false,
            None
          ),
          "2017"
        )
      )
    )

    testFieldComponent.getOptionalCurrentValue(mkHtmlFieldId("year")).get.equalsIgnoreCase("2017") shouldBe true
  }

}
