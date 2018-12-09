package driver.arangodb

import java.lang

import com.arangodb._
import com.arangodb.entity._
import com.arangodb.model._
import driver.DriverCommonConfig
import javax.inject.{Inject, Singleton}
import macros.{ArangoDoc, ArangoMapper}
import play.api.Configuration
import play.api.inject.ApplicationLifecycle

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.compat.java8.FutureConverters._
import scala.collection.JavaConverters._

/**
  * Class represents function to work with ArangoDB async client. Also this class integrated with play modules lifecycle
  * @param configuration client configuration
  * @param lifecycle play modules lifecycle
  */
@Singleton
class ArangoClient @Inject()(configuration: Configuration, lifecycle: ApplicationLifecycle)  {
  private val client = createClient

  /**
    * Registering stop hook (called when play will stops this module) to freeing connections and threads
    */
  lifecycle.addStopHook { () =>
    Future.successful {
      Try {
        client.shutdown()
      } match {
        case Success(_) => println("ArangoDB resources successfully closed")
        case Failure(exception) => println (s"Error to close resources $exception")
      }
    }
  }

  /**
    * Function creates arangodb client from configuration
    * @return asyncclient
    */
  private def createClient = {
    val config = new DriverCommonConfig(new ArangoDBAsync.Builder(), configuration.get[Configuration]("arangodb"))

    config
      .readValue[Boolean]("acquireHostList", {
        case (builder, Some(flag)) => builder.acquireHostList(flag)
      })
      .readHosts("hosts", {
        case (builder, hosts) => hosts.foreach(host => builder.host(host._1, host._2))
      })
      .readValue[String]("loadBalancing", {
        case (builder, Some(lb:String)) => builder.loadBalancingStrategy(LoadBalancingStrategy.valueOf(lb))
      })
      .readValue[Int]("timeout", {
        case (builder, Some(timeout:Int)) => builder.timeout(timeout)
      })
      .readValue[String]("user", {
        case (builder, Some(user:String)) => builder.user(user)
      })
      .readValue[String]("password", {
        case (builder, Some(password:String)) => builder.password(password)
      })
      .readValue[String]("password", {
        case (builder, Some(password:String)) => builder.password(password)
      })
      .readValue[Boolean]("useSsl", {
        case (builder, Some(flag:Boolean)) => builder.useSsl(flag)
      })
      .readValue[Int]("chunksize", {
        case (builder, Some(chunksize:Int)) => builder.chunksize(chunksize)
      })
      .readValue[Long]("connections.TTL", {
        case (builder, Some(ttl:Long)) => builder.connectionTtl(ttl)
      })
      .readValue[Int]("connections.max", {
        case (builder, Some(max:Int)) => builder.maxConnections(max)
      })
      .get.build()
  }

  /**
    * Function retrieves list of databases
    * @param ec execution context
    * @return database list
    */
  def databaseList(implicit ec: ExecutionContext): Future[Iterable[String]] = client.getDatabases.toScala.flatMap(list => Future.successful(list.asScala))

  /**
    * Function retrieves wrapper for concrete database functionality object
    * @param db name of desired database. If the name is not provided the value of play.database.arangodb will be used as database name
    * @return database wrapper instance
    */
  def db(db: String = null): ArangoClientDatabase =
    new ArangoClientDatabase(client.db(if (db != null && db.nonEmpty) db else configuration.get[String]("play.database.arangodb")))

  /**
    * Function retrieves wrapped instance of [[com.arangodb.ArangoDBAsync]]
    * @return db instance
    */
  def get: ArangoDBAsync = client
}

/**
  * Class is the wrapper for java [[com.arangodb.ArangoDatabaseAsync]] class
  * @param database arangodb database java instance
  */
class ArangoClientDatabase(database: ArangoDatabaseAsync) {
  def get: ArangoDatabaseAsync = database
  def create (db: String)(implicit ec: ExecutionContext): Future[Boolean] =
    database.exists().toScala.flatMap(
      flag => if (!flag) database.create().toScala.map(f => f.booleanValue()) else Future.successful(true)
    )

  def version: Future[ArangoDBVersion] = database.getVersion.toScala
  def exists(implicit ec: ExecutionContext): Future[Boolean] = database.exists().toScala.map(_.booleanValue())
  def info: Future[DatabaseEntity] = database.getInfo.toScala
  def drop(implicit ec: ExecutionContext): Future[Boolean] = database.drop.toScala.map(f => f.booleanValue())
  def collection(name: String) = new ArangoClientCollection(database.collection(name))
  def collectionList(implicit ec: ExecutionContext): Future[Iterable[CollectionEntity]] = database.getCollections().toScala.map(_.asScala)
  def query(query: String, bindVars: Map[String, AnyRef])(implicit options: AqlQueryOptions = null): Future[ArangoCursorAsync[BaseDocument]] = database.query(query, bindVars.asJava, options, classOf[BaseDocument]).toScala
  def graph(name: String): ArangoClientGraph = new ArangoClientGraph(database.graph(name))
}

/**
  * Class is the wrapper for [[com.arangodb.ArangoCollectionAsync]]
  * @param coll arangodb collection java instance
  */
class ArangoClientCollection(coll: ArangoCollectionAsync) {
  def get: ArangoCollectionAsync = coll
  def info: Future[CollectionEntity] = coll.getInfo.toScala
  def properties: Future[CollectionPropertiesEntity] = coll.getProperties.toScala
  def revision: Future[CollectionRevisionEntity] = coll.getRevision.toScala
  def indexList(implicit ec: ExecutionContext): Future[Iterable[IndexEntity]] = coll.getIndexes.toScala.map(_.asScala)
  def exists(implicit ec: ExecutionContext): Future[Boolean] = coll.exists().toScala.map(_.booleanValue())
  def documentExists(key: String): Future[lang.Boolean] = coll.documentExists(key).toScala

  def insert[T](doc: T)(implicit aw:ArangoMapper[T], opts: DocumentCreateOptions = null): Future[DocumentCreateEntity[BaseDocument]] =
    coll.insertDocument(aw(doc), opts).toScala

  def insertMany[T](docList: List[T])(implicit aw:ArangoMapper[T], opts: DocumentCreateOptions = null): Future[MultiDocumentEntity[DocumentCreateEntity[BaseDocument]]] = {
    val docs = docList.map(aw(_)).asJava
    coll.insertDocuments(docs, opts).toScala
  }

  def getDocument[T](key: String)(implicit aw:ArangoMapper[T], ec: ExecutionContext, ro: DocumentReadOptions = null): Future[ArangoDoc[T]] =
    coll.getDocument(key, classOf[BaseDocument], ro).toScala.map(rec => aw.fromBase(rec))

  def getDocumentList[T](keyList: List[String])(implicit aw:ArangoMapper[T], ec: ExecutionContext, ro: DocumentReadOptions = null): Future[Iterable[ArangoDoc[T]]] =
    coll.getDocuments(keyList.asJava, classOf[BaseDocument], ro).toScala.map (_.getDocuments.asScala.map(aw.fromBase))

  def replaceDocument[T](key: String, doc: T)(implicit aw:ArangoMapper[T], opts: DocumentReplaceOptions = null): Future[DocumentUpdateEntity[BaseDocument]] =
    coll.replaceDocument(key, aw(doc), opts).toScala

  def updateDocument[T](key: String, doc: T)(implicit aw:ArangoMapper[T], opts: DocumentUpdateOptions = null): Future[DocumentUpdateEntity[BaseDocument]] =
    coll.updateDocument(key, aw(doc), opts).toScala

  def deleteDocument(key: String): Future[DocumentDeleteEntity[Void]] = coll.deleteDocument(key).toScala

  def deleteDocumentList(key: List[String]): Future[MultiDocumentEntity[DocumentDeleteEntity[Void]]] =
    coll.deleteDocuments(key.asJava).toScala

  def truncate: Future[CollectionEntity] = coll.truncate().toScala
  def drop: Future[Void] = coll.drop().toScala
  def count: Future[CollectionPropertiesEntity] = coll.count().toScala
}

/**
  * Class is the wrapper for [[com.arangodb.ArangoCursorAsync]] with conversion to specified case class
  * @param cursor wrapped object with [[com.arangodb.entity.BaseDocument]] argument
  * @tparam T type of case class
  */
class ArangoClientCursor[T](cursor: ArangoCursorAsync[BaseDocument]) {
  import scala.compat.java8.StreamConverters._

  def toSeq(implicit aw:ArangoMapper[T]): Seq[ArangoDoc[T]] = cursor.streamRemaining().toScala[Seq].map(aw.fromBase)
  def toList(implicit aw:ArangoMapper[T]): List[ArangoDoc[T]] = cursor.streamRemaining().toScala[List].map(aw.fromBase)
  def next(implicit aw:ArangoMapper[T]): ArangoDoc[T] = aw.fromBase(cursor.next())
  def hasNext: Boolean = cursor.hasNext
  def close(): Unit = cursor.close()
  def count: Long = cursor.count()
}

/**
  * Object provides factory to build and disassemble [[com.arangodb.entity.EdgeDefinition]] class.
  * @example
  * {{{
  *   val edgeDefinition = EdgeDefinition("users", Seq("users"), Seq("roles"))
  *   val EdgeDefinition(col, from, to) = edgeDefinition
  *   edgeDefinition match {
  *     case edef @ EdgeDefinition(col, from, to) => ...
  *   }
  * }}}
  */
object EdgeDefinition {
  def apply(collection: String): EdgeDefinition = new EdgeDefinition().collection(collection)
  def apply(collection: String, from: Seq[String], to: Seq[String]): EdgeDefinition = new EdgeDefinition().collection(collection).from(from:_*).to(to:_*)
  def unapply(edgeDef: EdgeDefinition): Option[(String, Seq[String], Seq[String])] =
    if (edgeDef != null)
      Some(edgeDef.getCollection, edgeDef.getFrom.asScala.toSeq, edgeDef.getTo.asScala.toSeq)
    else
      None
}

/**
  * Class - scala wrapper for [[com.arangodb.ArangoGraphAsync]]
  * @param graph ArangoGraphAsync instance
  */
class ArangoClientGraph(graph: ArangoGraphAsync) {
  def get: ArangoGraphAsync = graph
  def drop(dropCollection: Boolean = false): Future[Void] = graph.drop(dropCollection).toScala
  def exists: Future[lang.Boolean] = graph.exists().toScala
}