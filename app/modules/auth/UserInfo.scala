package modules.auth

import com.mohiva.play.silhouette.api.{Authorization, Env, Identity, LoginInfo}
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticator
import macros.AeMacroCodeGen
import play.api.mvc.Request

import scala.concurrent.Future

object Roles extends Enumeration {
  @AeMacroCodeGen(
    bin ="""new Bin("%s", %s.name)""",
    fromRec = """modules.auth.Roles.fromString(%s.asInstanceOf[String])"""
  )
  trait Role {
    val name: String
  }

  protected case class BaseRole(name: String) extends super.Val with Role

  implicit def valueToPlanetRoleVal(x: Value): BaseRole = x.asInstanceOf[BaseRole]

  val Unknown = BaseRole("unknown")
  val AdminRole = BaseRole("admin")
  val UserRole = BaseRole("user")

  def fromString(name: String) = Roles.values.find(_.name == name).getOrElse(Unknown)
}

case class UserInfo (
  userId: String,
  loginInfo: LoginInfo,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  passwordInfo: Option[PasswordInfo],
  role: Roles.Role = Roles.UserRole
) extends Identity

trait DefaultEnv extends Env {
  type I = UserInfo
  type A = JWTAuthenticator
}

case class WithRole(role: Roles.Role) extends Authorization[DefaultEnv#I, DefaultEnv#A] {
  override def isAuthorized[B](user: DefaultEnv#I, authenticator: DefaultEnv#A)(implicit request: Request[B]): Future[Boolean] =
    Future.successful(user.role == role)
}