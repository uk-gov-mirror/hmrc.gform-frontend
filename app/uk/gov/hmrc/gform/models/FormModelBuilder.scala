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

import cats.data.NonEmptyList
import cats.{ Functor, MonadError }
import cats.syntax.all._

import scala.language.higherKinds
import scala.util.matching.Regex
import uk.gov.hmrc.gform.controllers.{ AuthCache, CacheData }
import uk.gov.hmrc.gform.eval.{ BooleanExprEval, DateExprEval, EvaluationContext, ExpressionResult, FileIdsWithMapping, RevealingChoiceInfo, StaticTypeInfo, SumInfo, TypeInfo }
import uk.gov.hmrc.gform.gform.{ FormComponentUpdater, PageUpdater }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelRenderPageOptics, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SmartString, SourceOrigin, SubmissionRef, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.http.HeaderCarrier

object FormModelBuilder {
  def fromCache[E, F[_]: Functor](
    cache: AuthCache,
    cacheData: CacheData,
    recalculation: Recalculation[F, E],
    componentIdToFileId: FormComponentIdToFileIdMapping
  )(implicit
    hc: HeaderCarrier,
    me: MonadError[F, E]
  ): FormModelBuilder[E, F] =
    new FormModelBuilder(
      cache.retrievals,
      cache.formTemplate,
      cacheData.thirdPartyData,
      cacheData.envelopeId,
      cache.accessCode,
      recalculation,
      componentIdToFileId
    )

  def evalIncludeIf[T <: PageMode](
    includeIf: IncludeIf,
    recalculationResult: RecalculationResult,
    recData: RecData[SourceOrigin.Current],
    formModel: FormModel[T],
    phase: Option[FormPhase]
  ): Boolean = {
    def compare(expr1: Expr, expr2: Expr, f: (ExpressionResult, ExpressionResult) => Boolean): Boolean = {
      val typeInfo1 = formModel.toFirstOperandTypeInfo(expr1)
      val typeInfo2 = formModel.toFirstOperandTypeInfo(expr2)
      val r = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo1, recData, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo1)
      val s = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo2, recData, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo2)
      f(r, s)
    }

    def compareDate(
      dateExprLHS: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr,
      dateExprRHS: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr,
      f: (DateResult, DateResult) => Boolean
    ): Boolean = {
      val evalFunc: uk.gov.hmrc.gform.sharedmodel.formtemplate.DateExpr => Option[DateResult] =
        DateExprEval
          .eval(
            formModel,
            recData.asInstanceOf[RecData[OutOfDate]],
            recalculationResult.evaluationContext,
            recalculationResult.evaluationResults
          )
      val exprResultLHS = evalFunc(dateExprLHS)
      val exprResultRHS = evalFunc(dateExprRHS)
      (exprResultLHS, exprResultRHS) match {
        case (Some(left), Some(right)) => f(left, right)
        case _                         => false
      }
    }

    def matchRegex(formCtx: FormCtx, regex: Regex): Boolean = {
      val typeInfo1 = formModel.toFirstOperandTypeInfo(formCtx)
      val expressionResult = recalculationResult.evaluationResults
        .evalExprCurrent(typeInfo1, recData, recalculationResult.evaluationContext)
        .applyTypeInfo(typeInfo1)

      expressionResult.matchRegex(regex)
    }

    def loop(booleanExpr: BooleanExpr): Boolean = booleanExpr match {
      case Equals(field1, field2)              => compare(field1, field2, _ identical _)
      case GreaterThan(field1, field2)         => compare(field1, field2, _ > _)
      case DateAfter(field1, field2)           => compareDate(field1, field2, _ after _)
      case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _)
      case LessThan(field1, field2)            => compare(field1, field2, _ < _)
      case DateBefore(field1, field2)          => compareDate(field1, field2, _ before _)
      case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _)
      case Not(invertedExpr)                   => !loop(invertedExpr)
      case Or(expr1, expr2)                    => val e1 = loop(expr1); val e2 = loop(expr2); e1 | e2
      case And(expr1, expr2)                   => val e1 = loop(expr1); val e2 = loop(expr2); e1 & e2
      case IsTrue                              => true
      case IsFalse                             => false
      case Contains(field1, field2)            => compare(field1, field2, _ contains _)
      case in @ In(_, _)                       => BooleanExprEval.evalInExpr(in, formModel, recalculationResult, recData)
      case MatchRegex(formCtx, regex)          => matchRegex(formCtx, regex)
      case FormPhase(value)                    => phase.fold(false)(_.value == value)
    }

    loop(includeIf.booleanExpr)

  }

  private def toCurrentData(
    modelComponentId: ModelComponentId,
    expressionResult: ExpressionResult
  ): VariadicFormData[SourceOrigin.Current] =
    expressionResult match {
      case ExpressionResult.Empty      => VariadicFormData.one[SourceOrigin.Current](modelComponentId, "")
      case ExpressionResult.Hidden     => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.Invalid(_) => VariadicFormData.empty[SourceOrigin.Current]
      case ExpressionResult.NumberResult(bigDecimal) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, bigDecimal.toString)
      case ExpressionResult.StringResult(value) => VariadicFormData.one[SourceOrigin.Current](modelComponentId, value)
      case ExpressionResult.OptionResult(value) =>
        VariadicFormData.many[SourceOrigin.Current](modelComponentId, value.map(_.toString))
      case d @ ExpressionResult.DateResult(_) =>
        VariadicFormData.one[SourceOrigin.Current](modelComponentId, d.asString)
    }

}

class FormModelBuilder[E, F[_]: Functor](
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  thirdPartyData: ThirdPartyData,
  envelopeId: EnvelopeId,
  maybeAccessCode: Option[AccessCode],
  recalculation: Recalculation[F, E],
  componentIdToFileId: FormComponentIdToFileIdMapping
)(implicit
  hc: HeaderCarrier,
  me: MonadError[F, E]
) {

  private def toRecalculationResults(
    data: VariadicFormData[SourceOrigin.OutOfDate],
    formModel: FormModel[Interim],
    formPhase: Option[FormPhase]
  ): F[RecalculationResult] = {
    val evaluationContext =
      new EvaluationContext(
        formTemplate._id,
        SubmissionRef(envelopeId),
        maybeAccessCode,
        retrievals,
        thirdPartyData,
        formTemplate.authConfig,
        hc,
        formPhase,
        FileIdsWithMapping(formModel.allFileIds, componentIdToFileId),
        formModel.dateLookup
      )

    recalculation
      .recalculateFormDataNew(data, formModel, formTemplate, retrievals, thirdPartyData, evaluationContext)
  }

  def dependencyGraphValidation[U <: SectionSelectorType: SectionSelector]: FormModel[DependencyGraphVerification] =
    expand(VariadicFormData.empty[SourceOrigin.OutOfDate])

  def renderPageModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase]
  ): FormModelOptics[D] = {

    implicit val fmvo = formModelVisibilityOptics

    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData
    val dataOutOfDate = data.asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]]
    val formModel: FormModel[DataExpanded] = expand(dataOutOfDate)
    val formModelVisibility: FormModel[Visibility] = visibilityModel(formModel, formModelVisibilityOptics, phase)

    val formModelVisibilityOpticsFinal = new FormModelVisibilityOptics[D](
      formModelVisibility,
      formModelVisibilityOptics.recData,
      formModelVisibilityOptics.recalculationResult
    )

    val formModelRenderPageOptics = FormModelRenderPageOptics[D](
      formModel,
      formModelVisibilityOptics.recData
    )

    FormModelOptics[D](formModelRenderPageOptics, formModelVisibilityOpticsFinal)
  }

  private def visibilityModel[D <: DataOrigin](
    formModel: FormModel[DataExpanded],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    phase: Option[FormPhase]
  ): FormModel[Visibility] = {
    val data: VariadicFormData[SourceOrigin.Current] = formModelVisibilityOptics.recData.variadicFormData

    FormComponentVisibilityFilter(formModelVisibilityOptics, phase)
      .stripHiddenFormComponents(formModel)
      .filter { pageModel =>
        pageModel.getIncludeIf.fold(true) { includeIf =>
          FormModelBuilder.evalIncludeIf(
            includeIf,
            formModelVisibilityOptics.recalculationResult,
            formModelVisibilityOptics.recData,
            formModelVisibilityOptics.formModel,
            phase
          )
        }
      }
      .map[Visibility] { singleton: Singleton[DataExpanded] =>
        val updatedFields = singleton.page.fields.flatMap {
          case fc @ IsRevealingChoice(rc) => fc.copy(`type` = RevealingChoice.slice(fc.id)(data)(rc)) :: Nil
          case otherwise                  => otherwise :: Nil
        }
        singleton.copy(page = singleton.page.copy(fields = updatedFields))
      } { repeater: Repeater[DataExpanded] =>
        repeater.asInstanceOf[Repeater[Visibility]]
      }
  }

  def visibilityModel[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate],
    phase: Option[FormPhase]
  ): F[FormModelVisibilityOptics[D]] = {
    val formModel: FormModel[Interim] = expand(data)

    val recalculationResultF: F[RecalculationResult] = toRecalculationResults(data, formModel, phase)

    recalculationResultF.map { recalculationResult =>
      buildFormModelVisibilityOptics(
        data,
        formModel,
        recalculationResult,
        phase
      )
    }
  }

  private def buildFormModelVisibilityOptics[U <: SectionSelectorType: SectionSelector, D <: DataOrigin](
    data: VariadicFormData[OutOfDate],
    formModel: FormModel[Interim],
    recalculationResult: RecalculationResult,
    phase: Option[FormPhase]
  ): FormModelVisibilityOptics[D] = {
    val evaluationResults = recalculationResult.evaluationResults
    val visibilityFormModel: FormModel[Visibility] = formModel.filter[Visibility] { pageModel =>
      pageModel.getIncludeIf.fold(true) { includeIf =>
        FormModelBuilder.evalIncludeIf(
          includeIf,
          recalculationResult,
          RecData(data).asInstanceOf[RecData[SourceOrigin.Current]],
          formModel,
          phase
        )
      }
    }

    val visibleTypedExprs: List[(FormComponentId, TypeInfo)] = visibilityFormModel.allFormComponents.collect {
      case fc @ HasValueExpr(expr) if !fc.editable => (fc.id, visibilityFormModel.explicitTypedExpr(expr, fc.id))
    }

    val visibleVariadicData: VariadicFormData[SourceOrigin.Current] =
      visibleTypedExprs.foldMap { case (fcId, typeInfo) =>
        val expressionResult =
          evaluationResults
            .evalExpr(typeInfo, RecData(data), recalculationResult.evaluationContext)
            .applyTypeInfo(typeInfo)

        FormModelBuilder.toCurrentData(fcId.modelComponentId, expressionResult)
      }

    val currentData = data ++ visibleVariadicData

    val recData: RecData[SourceOrigin.Current] = RecData.empty.copy(variadicFormData = currentData)

    FormModelVisibilityOptics[D](visibilityFormModel, recData, recalculationResult)
  }

  private def expand[T <: PageMode: FormModelExpander, U <: SectionSelectorType: SectionSelector](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] = {
    val basicFm: FormModel[T] = basic(data)
    mkFormModel(basicFm, data)
  }

  private def mkRepeater[T <: PageMode](s: Section.AddToList, index: Int): Repeater[T] = {
    val expand: SmartString => SmartString = _.expand(index, s.allIds)
    val fc = new FormComponentUpdater(s.addAnotherQuestion, index, s.allIds).updatedWithId
    Repeater[T](
      expand(s.title),
      expand(s.description),
      expand(s.shortName),
      expand(s.summaryName),
      s.includeIf,
      fc,
      index,
      s.instruction
    )
  }

  private def mkSingleton(page: Page[Basic], index: Int): Section.AddToList => Page[Basic] =
    source => PageUpdater(page, index, source.allIds)

  private def mergeIncludeIfs[T <: PageMode](includeIf: IncludeIf, page: Page[T]): Page[T] = page.copy(
    includeIf = Some(page.includeIf.fold(includeIf)(inIf => IncludeIf(And(inIf.booleanExpr, includeIf.booleanExpr))))
  )

  private def basicAddToList[T <: PageMode: FormModelExpander](
    s: Section.AddToList,
    index: Int,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Option[BracketPlain.AddToListIteration[T]] = {
    val singletons: List[Singleton[T]] =
      s.pages.map { page =>
        val page1: Page[Basic] = s.includeIf.fold(page)(includeIf => mergeIncludeIfs(includeIf, page))
        val page2: Page[Basic] = mkSingleton(page1, index)(s)
        val page3: Page[T] = implicitly[FormModelExpander[T]].lift(page2, data)
        Singleton[T](page3)
      }.toList

    val repeater: Repeater[T] = mkRepeater(s, index)

    NonEmptyList.fromList(singletons).map(BracketPlain.AddToListIteration(_, repeater))

  }

  private def basic[T <: PageMode, U <: SectionSelectorType](
    data: VariadicFormData[SourceOrigin.OutOfDate]
  )(implicit formModelExpander: FormModelExpander[T], sectionIncluder: SectionSelector[U]): FormModel[T] = {

    val allSections: List[Section] = sectionIncluder.getSections(formTemplate)

    val staticTypeInfo: StaticTypeInfo =
      allSections.foldLeft(StaticTypeInfo.empty)(_ ++ _.staticTypeInfo)

    val revealingChoiceInfo: RevealingChoiceInfo =
      allSections.foldLeft(RevealingChoiceInfo.empty)(_ ++ _.revealingChoiceInfo)

    val brackets: List[BracketPlain[T]] = allSections
      .map {
        case s: Section.NonRepeatingPage =>
          val page = formModelExpander.lift(s.page, data)
          Some(BracketPlain.NonRepeatingPage(Singleton[T](page), s))
        case s: Section.RepeatingPage => formModelExpander.liftRepeating(s, data)
        case s: Section.AddToList =>
          basicAddToList(s, 1, data).map(atl => BracketPlain.AddToList(NonEmptyList.one(atl), s))
      }
      .collect { case Some(bracket) =>
        bracket
      }

    val sumInfo: SumInfo = allSections.foldLeft(SumInfo.empty)(_ ++ _.sumInfo)

    NonEmptyList
      .fromList(brackets)
      .fold(throw new IllegalArgumentException("Form must have at least one (visible) page")) {
        FormModel.fromPages(_, staticTypeInfo, revealingChoiceInfo, sumInfo)
      }

  }

  private def repeaterIsYes(
    modelComponentId: ModelComponentId,
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): Boolean = {
    val nextOne: Option[Seq[String]] = data.many(modelComponentId)
    val next = nextOne.toSeq.flatten
    next.contains("0")
  }

  private def answeredAddToListIterations[T <: PageMode: FormModelExpander](
    iteration: BracketPlain.AddToListIteration[T],
    data: VariadicFormData[SourceOrigin.OutOfDate],
    source: Section.AddToList
  ): NonEmptyList[BracketPlain.AddToListIteration[T]] = {
    def loop(
      repeater: Repeater[T],
      acc: NonEmptyList[BracketPlain.AddToListIteration[T]]
    ): NonEmptyList[BracketPlain.AddToListIteration[T]] =
      if (repeaterIsYes(repeater.addAnotherQuestion.modelComponentId, data)) {
        val maybeBracket = basicAddToList(source, repeater.index + 1, data)
        maybeBracket
          .map { bracket =>
            loop(bracket.repeater, acc ::: NonEmptyList.one(bracket))
          }
          .getOrElse(acc)
      } else {
        acc
      }
    loop(iteration.repeater, NonEmptyList.one(iteration))
  }

  private def mkFormModel[T <: PageMode: FormModelExpander](
    formModel: FormModel[T],
    data: VariadicFormData[SourceOrigin.OutOfDate]
  ): FormModel[T] =
    formModel.flatMapRepeater {
      case (NonEmptyList(iteration, Nil), source) => answeredAddToListIterations(iteration, data, source)
      case (iterations, _)                        => iterations
    }
}
