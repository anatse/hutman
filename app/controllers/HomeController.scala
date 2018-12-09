package controllers
import com.aerospike.client.policy.{Policy, WritePolicy}
import com.aerospike.client.query.IndexType
import driver.arangodb.{ArangoClient, ArangoClientCursor}
import javax.inject._
import macros.{AerospikeDBCred, ArangoMacros, ArangoMapper}
import modules.cache.{AeClient, AerospikeQuery}
import play.api.mvc._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.control.NonFatal

case class Nested(nestedFirst: Option[String], dbc: AerospikeDBCred, ls: List[Boolean])
case class Test(field1: String, fopt: Option[String], f2: List[String], f3: Nested, f4: Double)

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, ae: AeClient, arangoClient: ArangoClient)  extends AbstractController(cc) {
  import macros.AeMacros._

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val writePolicy: WritePolicy = null
  implicit val policy: Policy = null
  implicit val dbc: AerospikeDBCred = AerospikeDBCred("test", "test")

  implicit class IntContext(val sc: StringContext) {
    def i(args: Any*): Int = {
      val orig = sc.s(args: _*)
      orig.replace(" ", "").toInt
    }
  }

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action.async { implicit request: Request[AnyContent] =>
    import AerospikeQuery._
    import ArangoMacros._

    implicit val kwS = aeKeyCodec[String]
    implicit val kwD = aeKeyCodec[Double]
    implicit val bw = aeCaseClassCodec[Test]
    implicit val aw:ArangoMapper[Test] = convertDoc[Test]

    implicit val timeout:Duration = 1 second

    val db = arangoClient.db()
    db.create("hutman").map(cr => println(s"created: $cr"))
    db.info.map(info => println(s"db = ${info.getName}, ${info.getPath}, ${info.getId}"))

    val test = Test("Hello", Some("optional-value"), List("one", "two", "three", "x"), Nested(Some("newClass"), dbc, List(true, false)), 10.1004)
    val res = db.collection("users").insert(test).map(res => println(s"Insert result: ${res.getId}")).recover({
      case NonFatal(ex) => println (ex)
    })

    val insertResult = Await.result(res, 1 second)
    println (s"Result: $insertResult")

    val usersCol = db.collection("users")
    val getDocFuture = usersCol.getDocument[Test]("56366")
    val getDocResult = Await.result(getDocFuture, 1 second)
    println (s"getDocResult: $getDocResult")

    val res3 = usersCol.getDocumentList[Test](List("61070", "56366", "58780")).recover({
      case NonFatal(ex) => println (ex)
    })

    println ("Start generating ...")
    val itemsToStore = (1 to 10000).map(_ => test)
    println ("Finish")

    println ("Start saving ...")
    val curtime = System.currentTimeMillis()

    (1 to 100).foreach { index =>
      val storeFuture = usersCol.insertMany[Test](itemsToStore.toList).recover({
        case NonFatal(ex) => print(s"Error storing many items: $ex")
      })

      // Store
      println (s"index: $index")

      Await.result(storeFuture, 600 second)
    }

    println (s"Time spent: ${System.currentTimeMillis() - curtime}")

    val gdr3 = Await.result(res3, 1 second)
    println (s"gdr3: $gdr3")

//    val queryFuture = db.query("FOR u IN users RETURN u", Map.empty).map(cursor => {
//      val items = new ArangoClientCursor(cursor).toSeq
//      println(s"Found items: ${items.length}")
//      items.foreach(println)
//    }).recover({
//      case NonFatal(ex) => println(s"Error% $ex")
//    })

//    Await.result(queryFuture, 10 second)

    ae.dropIndex("field1_index").map { _ =>
      println("index dropped")
      ae.createIndex("field1_index", "field1", IndexType.STRING).map{_ =>
        println("index created")
        withQuery {
          select ("field1", "f2") from "test" where "field1" =:= "Hello" //and "fopt" =:= "optional"
        }.execute(ae).map { rec =>
          println (s"found records: $rec")
        }
      }
    }

    ae.put(100.01, Test("Hello", Some("optional"), List("one", "two", "three", "x"), Nested(Some("newClass"), dbc, List(true, false)), 12.1234567))
      .flatMap { key =>
        ae.getK[Test](key).map(test =>
          Ok(views.html.index(test.map(_.toString).getOrElse("Not found")))
        )
      }
  }
}
