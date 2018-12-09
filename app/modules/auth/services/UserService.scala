package modules.auth.services

import com.aerospike.client.policy.{Policy, WritePolicy}
import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService
import macros.{AeMacros, AerospikeDBCred}
import modules.auth.UserInfo
import modules.cache.AeClient

import scala.concurrent.Future

/**
  * Class stores and retrieves inforation about user
  * @param ae aerospike cache client
  */
class UserService(ae: AeClient) extends IdentityService[UserInfo] {
  import AeMacros._

  implicit val writePolicy: WritePolicy = new WritePolicy()
  implicit val policy: Policy = new Policy()
  implicit val dbc: AerospikeDBCred = AerospikeDBCred("test", "test")

  implicit val kwS = aeKeyCodec[String]
  implicit val bw = aeCaseClassCodec[UserInfo]

  override def retrieve(loginInfo: LoginInfo): Future[Option[UserInfo]] = {
    ae.get[String, UserInfo](loginInfo.providerID + ":" + loginInfo.providerKey)
  }
}
