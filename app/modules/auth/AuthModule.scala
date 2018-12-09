package modules.auth

import com.google.inject.{AbstractModule, Provides}
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticator
import modules.auth.services.UserService
import net.codingwell.scalaguice.ScalaModule

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

//
//trait Plus[A] {
//  def plus(a1: A, a2: A): A
//}

/**
  * Create authenticated module
  */
class AuthModule extends AbstractModule with ScalaModule {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  override def configure() {
    bind[Silhouette[DefaultEnv]].to[SilhouetteProvider[DefaultEnv]]
  }

//  def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)

  @Provides
  def provideEnvironment(
                        userService: UserService,
                        authenticatorService: AuthenticatorService[JWTAuthenticator],
                        eventBus: EventBus
                      ): Environment[DefaultEnv] = {

//    implicit val x = new Plus[Int] {
//      override def plus(a1: Int, a2: Int): Int = a1 + a2
//    }
//
//    plus (1, 2)

    Environment[DefaultEnv](
      userService,
      authenticatorService,
      Seq(),
      eventBus
    )
  }
}

