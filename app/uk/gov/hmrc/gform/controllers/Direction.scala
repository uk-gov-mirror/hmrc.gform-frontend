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

package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ Bracket, Visibility }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait Navigation {

  def formModelOptics: FormModelOptics[DataOrigin.Browser]

  val availableSectionNumbers: List[SectionNumber] =
    formModelOptics.formModelVisibilityOptics.formModel.availableSectionNumbers

  val minSectionNumber: SectionNumber = availableSectionNumbers.min(Ordering.by((_: SectionNumber).value))

  val addToListBrackets: List[Bracket.AddToList[Visibility]] =
    formModelOptics.formModelVisibilityOptics.formModel.brackets.addToListBrackets

  val addToListSectionNumbers: List[SectionNumber] =
    addToListBrackets.flatMap(_.toPageModelWithNumber.toList).map(_._2)

  val addToListRepeaterSectionNumbers: List[SectionNumber] =
    addToListBrackets.flatMap(_.iterations.toList).map(_.repeater.sectionNumber)

  val addToListNonRepeaterSectionNumbers: List[SectionNumber] =
    addToListSectionNumbers.filterNot(addToListRepeaterSectionNumbers.toSet)

  val samePageRepeatersSectionNumbers: List[List[SectionNumber]] =
    formModelOptics.formModelVisibilityOptics.formModel.brackets.addToListBrackets
      .map(_.iterations.toList.map(_.repeater.sectionNumber))

  val filteredSectionNumbers: SectionNumber => List[SectionNumber] = sectionNumber =>
    if (addToListRepeaterSectionNumbers.contains(sectionNumber)) {
      val excludesAddToListNonRepeaterSectionNumbers = availableSectionNumbers
        .filterNot(addToListNonRepeaterSectionNumbers.toSet)

      samePageRepeatersSectionNumbers
        .find(_.contains(sectionNumber))
        .fold(excludesAddToListNonRepeaterSectionNumbers) { l =>
          excludesAddToListNonRepeaterSectionNumbers.filterNot(l.toSet)
        }
    } else
      availableSectionNumbers
}

// TODO: Origin should not be in controllers, but Navigator probably should!
case class Origin(formModelOptics: FormModelOptics[DataOrigin.Browser]) extends Navigation

sealed trait Direction

case object SaveAndContinue extends Direction
case object Back extends Direction
case object SaveAndExit extends Direction
case object Exit extends Direction
case object SummaryContinue extends Direction
case object Continue extends Direction
case class AddGroup(modelComponentId: ModelComponentId) extends Direction
case class RemoveGroup(modelComponentId: ModelComponentId) extends Direction
case class RemoveAddToList(idx: Int, addToListId: AddToListId) extends Direction
case class EditAddToList(idx: Int, addToListId: AddToListId) extends Direction

case class Navigator(
  sectionNumber: SectionNumber,
  requestRelatedData: RequestRelatedData,
  formModelOptics: FormModelOptics[DataOrigin.Browser]
) extends Navigation {
  require(
    sectionNumber >= minSectionNumber,
    s"section number is too low: ${sectionNumber.value} is not >= $minSectionNumber"
  )
  require(
    sectionNumber <= maxSectionNumber,
    s"section number is too big: ${sectionNumber.value} is not <= $maxSectionNumber"
  )

  private lazy val maxSectionNumber: SectionNumber = availableSectionNumbers.max(Ordering.by((_: SectionNumber).value))

  val previousOrCurrentSectionNumber: SectionNumber =
    filteredSectionNumbers(sectionNumber).reverse
      .find(_ < sectionNumber)
      .getOrElse(sectionNumber)

}
