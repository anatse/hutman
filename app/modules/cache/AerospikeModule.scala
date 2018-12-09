package modules.cache

import com.aerospike.client.async.{EventPolicy, NioEventLoops, AsyncClient => AerospikeAsyncClient, AsyncClientPolicy => AerospikeAsyncClientPolicy}
import com.aerospike.client.listener.{RecordListener, RecordSequenceListener, WriteListener}
import com.aerospike.client.policy.{Policy, QueryPolicy, WritePolicy}
import com.aerospike.client.query.{IndexType, Filter => AeFilter, Statement => AeStmt}
import com.aerospike.client.{AerospikeException, Bin, Key, Record, Host => AerospikeHost}
import com.typesafe.config.Config
import javax.inject.Inject
import macros.{AerospikeCaseClassWrapper, AerospikeDBCred, AerospikeKeyWrapper}
import play.api.{Configuration, Environment}
import play.api.inject.{ApplicationLifecycle, Binding, Module}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * Play 2 module for aerospike cache. Module use aerospike macros to convert keys and bins to/from case classes
  * and simple scala types.
  * Module also integrated with Play lifecycle.
  */
@javax.inject.Singleton
class AerospikeModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = Seq(
    bind[AeClient].toSelf
  )
}

/**
  * Wrapper for aerospike cache
  * @param client aerospike client
  * @param namespace default namespace
  */
@javax.inject.Singleton
class AeClient @Inject()(configuration: Configuration, lifecycle: ApplicationLifecycle) {
  val (eventLoops, client) = createClient

  /**
    * Function read hosts from aerospike config
    * @param rootConfig root config
    * @param path path with hosts
    * @return list of hosts
    */
  def readHosts (rootConfig: Config, path: String) = {
    rootConfig.getConfigList(path).asScala.map(obj => new AerospikeHost(
      obj.getString("host"),
      obj.getInt("port")
    )).toList
  }

  /**
    * Registering stop hook (called when play will stops this module) to freeing connections and threads
    */
  lifecycle.addStopHook { () =>
    Future.successful {
      Try {
        eventLoops.close()
        client.close()
      } match {
        case Success(_) => println("Aerospike resources successfully closed")
        case Failure(exception) => println (s"Error to close resources ${exception}")
      }
    }
  }

  /**
    * Function opens the connection to aerospike nodes
    * @return tuple with EventLoops and AsyncClient objects
    */
  private def createClient = {
    val aerospike = configuration.get[Configuration]("aerospike")
    val eventPolicy = new EventPolicy
    val policy = new AerospikeAsyncClientPolicy
    policy.readPolicyDefault.socketTimeout = aerospike.get[Int]("policy.readPolicyDefault.socketTimeout")
    policy.readPolicyDefault.totalTimeout = aerospike.get[Int]("policy.readPolicyDefault.totalTimeout")
    policy.readPolicyDefault.sleepBetweenRetries = aerospike.get[Int]("policy.readPolicyDefault.sleepBetweenRetries")
    policy.writePolicyDefault.socketTimeout = aerospike.get[Int]("policy.writePolicyDefault.socketTimeout")
    policy.writePolicyDefault.totalTimeout = aerospike.get[Int]("policy.writePolicyDefault.totalTimeout")
    policy.writePolicyDefault.sleepBetweenRetries = aerospike.get[Int]("policy.writePolicyDefault.sleepBetweenRetries")
    policy.eventLoops = new NioEventLoops(eventPolicy, aerospike.get[Int]("policy.eventLoops.nioEventLoops"))
    (policy.eventLoops, new AerospikeAsyncClient(policy, readHosts(aerospike.underlying, "hosts"):_*))
  }

  /**
    * Function put value into aerospike cache in asynchronous manner with given key
    * @param policy write policy
    * @param key key
    * @param bins data
    * @return future with key
    */
  private def putFuture (policy: WritePolicy, key: Key, bins: Seq[Bin]):Future[Key] = {
    val promise = Promise[Key]()

    val listener = new WriteListener {
      override def onSuccess(key: Key): Unit = promise.success(key)
      override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)
    }

    try {
      client.put(policy, listener, key, bins: _*)
    } catch {
      case NonFatal(e) => promise.failure(e)
    }

    promise.future
  }

  /**
    * Function get value from aerospike cache by given key
    * @param policy read policy
    * @param key key
    * @param convert data convert function
    * @tparam BIN type of data bin
    * @return future with optional data object
    */
  private def getFuture[BIN](policy: Policy, key: Key)(convert:Record => BIN):Future[Option[BIN]] = {
    val promise = Promise[Option[BIN]]()

    val listener = new RecordListener {
      override def onSuccess(key: Key, record: Record): Unit = Try(if (record == null) None else Some(convert.apply(record))) match {
        case Success(value) => promise.success(value)
        case Failure(exception) => promise.failure(exception)
      }
      override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)
    }

    try {
      client.get(policy, listener, key)
    } catch {
      case NonFatal(e) => promise.failure(e)
    }

    promise.future
  }

  /**
    * Create aerospike key
    * @param setName set name of aerospike keys
    * @param value key name
    * @return new created key
    */
  def createKey[T] (value: T)(implicit kw: AerospikeKeyWrapper[T]):Key = kw(value)

  /**
    * Create aerospike bin
    * @param value value to store
    * @param kw converter
    * @tparam T type of value
    * @return bin
    */
  def createBin[T] (value: T)(implicit kw: AerospikeCaseClassWrapper[T]):Seq[Bin] = kw(value)

  /**
    * Function puts value into aerospike. Performs the action in asynchronous manner
    * @param key key
    * @param bin data
    * @param kw key converter
    * @param bw data converter
    * @param policy write policy
    * @tparam KEY key type
    * @tparam BIN data type
    * @return future with put action
    */
  def put[KEY, BIN](key: KEY, bin: BIN)(implicit kw: AerospikeKeyWrapper[KEY], bw: AerospikeCaseClassWrapper[BIN], policy: WritePolicy):Future[Key] =
    putFuture(policy, kw(key), bw(bin))

  /**
    * Function puts value into aerospike, using as key aerospike key type
    * @param key key
    * @param bin bin
    * @param bw data to bin converter
    * @param policy write policy
    * @tparam BIN data type
    */
  def putK[BIN](key: Key, bin: BIN)(implicit bw: AerospikeCaseClassWrapper[BIN], policy: WritePolicy):Unit =
    putFuture(policy, key, bw(bin))

  /**
    * Function gets data by key
    * @param key key
    * @param policy read policy
    * @param kw key converter
    * @param bw data converter
    * @tparam KEY key type
    * @tparam BIN data type
    * @return converted found data
    */
  def get[KEY, BIN](key: KEY)(implicit policy: Policy, kw: AerospikeKeyWrapper[KEY], bw: AerospikeCaseClassWrapper[BIN]):Future[Option[BIN]] =
    getFuture(policy, kw(key))(bw.toValue)

  /**
    * Function gets data by native aerospike key
    * @param key key
    * @param policy read policy
    * @param kw key converter
    * @param bw data converter
    * @tparam KEY key type
    * @tparam BIN data type
    * @return converted found data
    */
  def getK[T](key: Key)(implicit policy: Policy, bw: AerospikeCaseClassWrapper[T]):Future[Option[T]] =
    getFuture(policy, key)(bw.toValue)

  /**
    * Function puts value into aerospike, using native aerospike key and native aerospike bin
    * @param key key
    * @param bin bin
    * @param policy write policy
    * @return future with key
    */
  def putNative(key: Key, bin: Bin)(implicit policy: WritePolicy): Future[Key] = putFuture(policy, key, Seq(bin))

  /**
    * Function synchronously gets data by native aerospike key
    * @param key key
    * @param policy read policy
    * @return found aerospike record
    */
  def getSyncNative(key: Key)(implicit policy: Policy): Record = client.get(policy, key)

  /**
    * function wraps aerospike query {@link com.aerospike.client.async.AsyncClient#query}
    * @param stmt statement {@link com.aerospike.client.query.Statement}
    * @return list of maps with key -> record
    */
  def query (stmt: AeStmt)(implicit queryPolicy: QueryPolicy = null): Future[List[Map[Key, Record]]] = {
    val promise = Promise[List[Map[Key, Record]]]()
    val listener: RecordSequenceListener = new RecordSequenceListener() {
      private val buf = ArrayBuffer[Map[Key, Record]]()
      override def onRecord(key: Key, record: Record): Unit = buf += Map(key -> record)
      override def onSuccess(): Unit = promise.success(buf.toList)
      override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)
    }

    try {
      client.query(queryPolicy, listener, stmt)
    } catch {
      case NonFatal(ex) => promise.failure(ex)
    }

    promise.future
  }

  /**
    * Function wraps {@link com.aerospike.client.async.AsyncClient#createIndex}
    * @param indexName index name
    * @param binName bin name
    * @param indexType type of index
    * @param policy policy
    * @param dbc database credentials (namespace and set)
    * @param ec execution context
    * @param timeout timeout
    * @return future which contains creation task
    */
  def createIndex (indexName: String, binName: String, indexType:IndexType)(implicit policy: Policy, dbc: AerospikeDBCred, ec:ExecutionContext, timeout: Duration): Future[Unit] = {
    Future[Unit] {
      val task = client.createIndex(policy, dbc.namespace, dbc.setName, indexName, binName, indexType)
      task.waitTillComplete(1000, timeout.toMillis.toInt)
    } recover {
      case NonFatal(e) => println (s"Error create index: $e")
    }
  }

  /**
    * Function wraps {@link com.aerospike.client.async.AsyncClient#dropIndex} which drops index with specified name
    * @param indexName index name
    * @param policy policy
    * @param dbc database credentials (namespace and set)
    * @param ec execution context
    * @param timeout timeout for the operation
    * @return future with drop task
    */
  def dropIndex (indexName: String)(implicit policy: Policy, dbc: AerospikeDBCred, ec:ExecutionContext, timeout: Duration): Future[Unit] = {
    Future[Unit] {
      val task = client.dropIndex(policy, dbc.namespace, dbc.setName, indexName)
      task.waitTillComplete(1000, timeout.toMillis.toInt)
    } recover {
      case NonFatal(e) => println (s"Error drop index: $e")
    }
  }
}

/**
  * Class represents DLS mini language for querying data from aerospike database
  * Realized sql-like queries
  */
class AerospikeQuery {
  private val filters = ArrayBuffer[AeFilter]()
  private val stmt = new AeStmt()

  def from(set: String): AerospikeQuery = {
    stmt.setSetName(set)
    this
  }

  def where(f: AeFilter): AerospikeQuery = {
    filters += f
    this
  }

  def and(f: AeFilter): AerospikeQuery = {
    filters += f
    this
  }

  def execute(ae:AeClient)(implicit dbc: AerospikeDBCred): Future[List[Map[Key, Record]]] = {
    stmt.setNamespace(dbc.namespace)
    if (filters.nonEmpty) stmt.setFilters(filters:_*)
    ae.query(stmt)
  }
}

/**
  * Implicit classes and object for query DSL for aerospike
  */
object AerospikeQuery {
  @inline implicit class StringFieldValue (val s: String) {
    def =:=(right:String): AeFilter = AeFilter.equal (s, right)
    def =:=(right:Int): AeFilter = AeFilter.equal (s, right)
    def =:=(right:Long): AeFilter = AeFilter.equal (s, right)
  }

  object select {
    def apply(names: String*)(implicit queryPolicy: QueryPolicy = null): AerospikeQuery = {
      val query = new AerospikeQuery()
      query.stmt.setBinNames(names:_*)
      query
    }
  }

  @inline def withQuery (f: => AerospikeQuery):AerospikeQuery = f
}
