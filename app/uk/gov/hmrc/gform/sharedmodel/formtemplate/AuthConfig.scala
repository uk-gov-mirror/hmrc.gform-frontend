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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LocalisedString, ValueClassFormat }

case class EnrolmentAuth(
  serviceId: ServiceId,
  enrolmentCheck: EnrolmentCheck
)
object EnrolmentAuth {
  implicit val format: OFormat[EnrolmentAuth] = derived.oformat()
}

sealed trait EnrolmentCheck extends Product with Serializable
case class DoCheck(
  enrolmentCheckPredicate: EnrolmentCheckPredicate,
  needEnrolment: NeedEnrolment,
  check: EnrolmentPostCheck
) extends EnrolmentCheck
case object Never extends EnrolmentCheck
object EnrolmentCheck {
  implicit val format: OFormat[EnrolmentCheck] = derived.oformat()
}

sealed trait EnrolmentCheckPredicate extends Product with Serializable
case object Always extends EnrolmentCheckPredicate
case object ForNonAgents extends EnrolmentCheckPredicate
object EnrolmentCheckPredicate {
  implicit val format: OFormat[EnrolmentCheckPredicate] = derived.oformat()
}

sealed trait NeedEnrolment extends Product with Serializable
case class RequireEnrolment(enrolmentSection: EnrolmentSection, enrolmentAction: EnrolmentAction) extends NeedEnrolment
case object RejectAccess extends NeedEnrolment
object NeedEnrolment {
  implicit val format: OFormat[NeedEnrolment] = derived.oformat()
}

sealed trait EnrolmentPostCheck extends Product with Serializable
case object NoCheck extends EnrolmentPostCheck
case class RegimeIdCheck(regimeId: RegimeId) extends EnrolmentPostCheck
object EnrolmentPostCheck {
  implicit val format: OFormat[EnrolmentPostCheck] = derived.oformat()
}

sealed trait EnrolmentCheckVerb extends Product with Serializable
case object NeverVerb extends EnrolmentCheckVerb
case object AlwaysVerb extends EnrolmentCheckVerb
case object ForNonAgentsVerb extends EnrolmentCheckVerb

object EnrolmentCheckVerb {

  private val never = "never"
  private val always = "always"
  private val forNonAgents = "forNonAgents"

  implicit val format: Format[EnrolmentCheckVerb] =
    ADTFormat.formatEnumeration(never -> NeverVerb, always -> AlwaysVerb, forNonAgents -> ForNonAgentsVerb)

  def asString(verb: EnrolmentCheckVerb): String = verb match {
    case NeverVerb        => never
    case AlwaysVerb       => always
    case ForNonAgentsVerb => forNonAgents
  }
}

sealed trait AuthConfig extends Product with Serializable {
  def isEmailAuthConfig = this match {
    case EmailAuthConfig(_) => true
    case _                  => false
  }
}

case object Anonymous extends AuthConfig
case object HmrcAny extends AuthConfig
case class HmrcVerified(ivFailure: LocalisedString, notAllowedIn: LocalisedString) extends AuthConfig
case object HmrcSimpleModule extends AuthConfig
case class HmrcEnrolmentModule(enrolmentAuth: EnrolmentAuth) extends AuthConfig
case class HmrcAgentModule(agentAccess: AgentAccess) extends AuthConfig
case class HmrcAgentWithEnrolmentModule(agentAccess: AgentAccess, enrolmentAuth: EnrolmentAuth) extends AuthConfig
case object AWSALBAuth extends AuthConfig
case class EmailAuthConfig(service: EmailVerifierService) extends AuthConfig

object HasEnrolmentSection {
  def unapply(ac: AuthConfig): Option[(ServiceId, EnrolmentSection, EnrolmentPostCheck, EnrolmentAction)] =
    ac match {
      case HmrcEnrolmentModule(EnrolmentAuth(serviceId, DoCheck(_, RequireEnrolment(es, enrolmentAction), check))) =>
        Some((serviceId, es, check, enrolmentAction))
      case HmrcAgentWithEnrolmentModule(
            _,
            EnrolmentAuth(serviceId, DoCheck(_, RequireEnrolment(es, enrolmentAction), check))
          ) =>
        Some((serviceId, es, check, enrolmentAction))
      case _ => None
    }
}

object AuthConfig {

  def toEnrolmentPostCheck(maybeRegimeId: Option[RegimeId]): EnrolmentPostCheck =
    maybeRegimeId.fold(NoCheck: EnrolmentPostCheck)(RegimeIdCheck.apply)

  def enrolmentActionMatch(enrolmentAction: Option[EnrolmentAction]): EnrolmentAction =
    enrolmentAction.getOrElse(NoAction)

  def toEnrolmentAuth(
    serviceId: ServiceId,
    maybeRegimeId: Option[RegimeId],
    maybeEnrolmentCheck: Option[EnrolmentCheckVerb],
    maybeEnrolmentSection: Option[EnrolmentSection],
    maybeEnrolmentAction: Option[EnrolmentAction]
  ): EnrolmentAuth =
    (maybeEnrolmentCheck, maybeEnrolmentSection) match {
      case (Some(AlwaysVerb), Some(enrolmentSection)) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(
            Always,
            RequireEnrolment(enrolmentSection, enrolmentActionMatch(maybeEnrolmentAction)),
            toEnrolmentPostCheck(maybeRegimeId)
          )
        )
      case (Some(ForNonAgentsVerb), Some(enrolmentSection)) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(
            ForNonAgents,
            RequireEnrolment(enrolmentSection, enrolmentActionMatch(maybeEnrolmentAction)),
            toEnrolmentPostCheck(maybeRegimeId)
          )
        )
      case (Some(AlwaysVerb), None) =>
        EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, toEnrolmentPostCheck(maybeRegimeId)))
      case (Some(ForNonAgentsVerb), None) =>
        EnrolmentAuth(serviceId, DoCheck(ForNonAgents, RejectAccess, toEnrolmentPostCheck(maybeRegimeId)))
      case (Some(NeverVerb) | None, _) => EnrolmentAuth(serviceId, Never)
    }

  implicit val format: OFormat[AuthConfig] = derived.oformat()

}

case class ServiceId(value: String) extends AnyVal
object ServiceId {
  implicit val format: OFormat[ServiceId] = ValueClassFormat.oformat("value", ServiceId.apply, _.value)
}

case class RegimeId(value: String) extends AnyVal
object RegimeId {
  implicit val format: Format[RegimeId] = ValueClassFormat.oformat("regimeId", RegimeId.apply, _.value)
}

sealed trait EnrolmentAction
case class LegacyFcEnrolmentVerifier(value: String) extends EnrolmentAction
case object NoAction extends EnrolmentAction

object EnrolmentAction {
  implicit val format: Format[EnrolmentAction] = derived.oformat[EnrolmentAction]()
}

object LegacyFcEnrolmentVerifier {
  implicit val format: Format[LegacyFcEnrolmentVerifier] =
    ValueClassFormat.oformat("legacyFcEnrolmentVerifier", LegacyFcEnrolmentVerifier.apply, _.value)
}

sealed trait AgentAccess
case object RequireMTDAgentEnrolment extends AgentAccess
case object DenyAnyAgentAffinityUser extends AgentAccess
case object AllowAnyAgentAffinityUser extends AgentAccess

object AgentAccess {
  val requireMTDAgentEnrolment = "requireMTDAgentEnrolment"
  val denyAnyAgentAffinityUser = "denyAnyAgentAffinityUser"
  val allowAnyAgentAffinityUser = "allowAnyAgentAffinityUser"

  implicit val format: Format[AgentAccess] = ADTFormat.formatEnumerationWithDefault(
    RequireMTDAgentEnrolment,
    Seq(RequireMTDAgentEnrolment, DenyAnyAgentAffinityUser, AllowAnyAgentAffinityUser).map(t => (asString(t) -> t)): _*
  )

  def asString(access: AgentAccess): String = access match {
    case RequireMTDAgentEnrolment  => requireMTDAgentEnrolment
    case DenyAnyAgentAffinityUser  => denyAnyAgentAffinityUser
    case AllowAnyAgentAffinityUser => allowAnyAgentAffinityUser
  }
}
