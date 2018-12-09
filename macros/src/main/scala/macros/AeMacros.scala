package macros

import com.aerospike.client.{Bin, Key, Record}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import MacroCommon._

/**
  * Base trait to wrap aerospike key wrappers
  * @tparam KEY type of key
  */
trait AerospikeKeyWrapper[KEY] {
  val namespace: String
  val setName: String
  def apply(k: KEY): Key
}

/**
  * Base trait for aerospike bin wrappers
  * @tparam T type of case class to wrap
  */
trait AerospikeCaseClassWrapper[T] {
  def apply(obj: T): Seq[Bin]
  def toValue(rec: Record): T
}

/**
  * Annotation class. Used at compile time to generate conversion code for non case classes (e.g. enum)
  * @example Enum for user roles
  * {{{
  *   // Enum class represents roles
  *   object Roles extends Enumeration {
  *     @MacroCodeGen(
  *       bin ="""new Bin("%s", %s.name)""",
  *       fromRec = """modules.auth.Roles.fromString(%s.asInstanceOf[String])"""
  *     )
  *     trait Role {
  *       val name: String
  *     }
  *
  *     protected case class BaseRole(name: String) extends super.Val with Role
  *
  *     implicit def valueToPlanetRoleVal(x: Value): BaseRole = x.asInstanceOf[BaseRole]
  *
  *     val Unknown = BaseRole("unknown")
  *     val AdminRole = BaseRole("admin")
  *     val UserRole = BaseRole("user")
  *
  *     def fromString(name: String) = Roles.values.find(_.name == name).getOrElse(Unknown)
  *   }
  * }}}
  * @param bin code to generate [[com.aerospike.client.Bin]] class
  * @param fromRec code to retrieve instance of the annotated class from [[com.aerospike.client.Record]]
  */
case class AeMacroCodeGen(bin: String, fromRec: String) extends StaticAnnotation

/**
  * Case class stores aerospike database parameters
  * @param namespace namespace (database)
  * @param setName name of aerospice set (e.g. user)
  */
case class AerospikeDBCred(namespace: String, setName: String)

/**
  * Object provides implicit functions which can be use to generate macros for Aerospike keys and bins. All the macros
  * generated at compile time.
  * <p>
  * For keys only primitives (include string) allowed
  * <p>
  * To generating macros for bins only case classes allowed. Not allowed Option fields in case classes. Nested case classed also supported
  * Top level case class fields put info aerospice set as bins, nested case classes as maps
  */
object AeMacros {
  /**
    * This functions provide codec to convert from primitive scala type to aerospike [[com.aerospike.client.Key]]
    * @example
    * {{{
    *   import AeMacros._
    *   implicit val keyStringCodec = aeKeyCodec[String]
    *   val userKey:Key = "userKey"
    * }}}
    * @param dbc database credebntials (namespace and set name)
    * @tparam T primitive type of key field
    * @return com.aerospike.client.Key object
    * @see AerospikeDBCred
    * @see com.aerospike.client.Key
    */
  def aeKeyCodec[T](implicit dbc: AerospikeDBCred): AerospikeKeyWrapper[T] = macro convertKeyImpl[T]

  /**
    * Implements macros to generate convert code from primitive to aerospike Key
    * @param c blackbox.Context
    * @param dbc aerospike database credentials
    * @tparam T type
    * @return wrapper for key of given type
    */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def convertKeyImpl[T: c.WeakTypeTag](c: blackbox.Context)(dbc: c.Expr[AerospikeDBCred]): c.Expr[AerospikeKeyWrapper[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val tpeSt = tpe.toString

    // Check is type applicable for conversion and convert
    val toValue:c.universe.Tree = tpe match {
      case x if x =:= weakTypeOf[String] => q"new StringValue(v)"
      case x if x =:= weakTypeOf[Int] => q"new IntegerValue(v)"
      case x if x =:= weakTypeOf[Boolean] => q"new BooleanValue(v)"
      case t if t =:= weakTypeOf[Float] => q"new FloatValue(v)"
      case t if t =:= weakTypeOf[Double] => q"new DoubleValue(v)"
      case t if t =:= weakTypeOf[Array[Byte]] => q"new BytesValue(v)"
      case _ => throw new Exception(s"Error compiling macro - wrong type for key: $tpeSt")
    }

    val ns = reify(dbc.splice.namespace)
    val sn = reify(dbc.splice.setName)

    c.Expr[AerospikeKeyWrapper[T]] {
      q"""
      import com.aerospike.client.{Key, Value}
      import com.aerospike.client.Value._

       new macros.AerospikeKeyWrapper[$tpe] {
         override val namespace = $ns
         override val setName = $sn
         override def apply(v: $tpe): Key = new Key(namespace, setName, $toValue)
       }"""
    }
  }

  /**
    * Implements macros to generate convert code from primitive to aerospike [[com.aerospike.client.Bin]]
    *
    * @example
    * {{{
    *   case class UserInfo (
    *     userId: String,
    *     loginInfo: LoginInfo,
    *     firstName: Option[String],
    *     lastName: Option[String],
    *     fullName: Option[String],
    *     email: Option[String]
    *   )
    *
    *   import AeMacros._
    *   implicit val binUserInfoCodec = aeCaseClassCodec[UserInfo]
    *   val userBin:Bin = UserInfo("demo", LoginInfo(...), None, None, None, None)
    * }}}
    * @tparam T case class type
    * @return bin
    */
  def aeCaseClassCodec[T]: AerospikeCaseClassWrapper[T] = macro convertCaseClassImpl[T]

  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def convertCaseClassImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[AerospikeCaseClassWrapper[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val tpeSt = tpe.toString

    // Collect all fields in case class constructor (should be only one)
    val fields = getFields(c)(tpe)

    val values = fields.map { field =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature.resultType
      // Generate code for apply method
      val value:String = returnType match {
        case x if x =:= weakTypeOf[String]  => s"""new Bin("$key", new StringValue(v.$key))"""
        case x if x =:= weakTypeOf[Int] => s"""new Bin("$key", new IntegerValue(v.$key))"""
        case x if x =:= weakTypeOf[Boolean] => s"""new Bin("$key", new BooleanValue(v.$key))"""
        case t if t =:= weakTypeOf[Float] => s"""new Bin("$key", new FloatValue(v.$key))"""
        case t if t =:= weakTypeOf[Double] => s"""new Bin("$key", new DoubleValue(v.$key))"""
        case t if t =:= weakTypeOf[Array[Byte]] => s"""new Bin("$key", new BytesValue(v.$key))"""

        // Lists
        case t if t <:< weakTypeOf[List[_]] => t.typeArgs.head match {
          // Primitives
          case ta if ta =:= weakTypeOf[String]
            || t <:< weakTypeOf[AnyVal]  => s"""new Bin("$key", new ListValue(v.$key.asJava))"""

          // List of case classes
          case ta if ta.typeArgs.head.typeSymbol.isClass
            && ta.typeArgs.head.typeSymbol.asClass.isCaseClass => s"""new Bin("$key", new ListValue(Map (""" +  pickValue (c)(t, s"v.$key") + s""").asJava))"""
        }

        // Optional values for primitives
        case t if t <:< weakTypeOf[Option[_]] => {
          val formatString =  s"""new Bin("$key", v.$key match {
                case Some(z) => new %sValue(z)
                case _ => null
              })"""

          t.typeArgs.head match {
            case ta if ta =:= weakTypeOf[String] => formatString.format("String")
            case ta if ta =:= weakTypeOf[Int] => formatString.format("Int")
            case ta if ta =:= weakTypeOf[Boolean] => formatString.format("Boolean")
            case ta if ta =:= weakTypeOf[Float] => formatString.format("Float")
            case ta if ta =:= weakTypeOf[Float] => formatString.format("Double")
            case ta if ta =:= weakTypeOf[Array[Byte]] => formatString.format("BytesValue")

            // Class
            case ta if ta.typeSymbol.isClass && ta.typeSymbol.asClass.isCaseClass =>
              s"""new Bin("$key", v.$key match {
                 case Some(obj) => Map(${pickValue (c)(ta, s"obj")}).asJava
                 case None => null
               })"""
          }
        }

        // Case class
        case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass => s"""new Bin("$key", Map(""" + pickValue (c)(t, s"v.$key") + ").asJava)"

        // Custom classes
        case t if contains(c)(t) =>
          val name = t.typeSymbol.annotations.filter(_.tree.tpe =:= weakTypeOf[AeMacroCodeGen]).head.tree.children.tail.head match {
            case Literal(Constant(value)) => value.asInstanceOf[String]
          }
          String.format (name, key, s"v.$key")

        // Error
        case _ =>
          throw new Exception(s"Error compiling macro - wrong type for field: $tpeSt.$key:  ${returnType.toString} ")
      }

      // Code for apply method
      c.parse(value)
    }

    // Generate code for toValue function
    val toValueCode = fields.zipWithIndex.foldLeft(s"$tpe(") { case (code, (field, index)) =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature.resultType
      code + (returnType match {
        case t if t =:= weakTypeOf[String]
          || t =:= weakTypeOf[Int]
          || t =:= weakTypeOf[Boolean]
          || t =:= weakTypeOf[Float]
          || t =:= weakTypeOf[Double]
          || t =:= weakTypeOf[Array[Byte]] => s"""$key = rec.bins.get("$key").asInstanceOf[$returnType]${addComma (index, fields.length)}"""

        case t if t <:< weakTypeOf[List[_]] => t.typeArgs.head match {
          case ta if ta =:= weakTypeOf[String]
            || t =:= weakTypeOf[Int]
            || t =:= weakTypeOf[Boolean]
            || t =:= weakTypeOf[Float]
            || t =:= weakTypeOf[Double] => s"""$key = rec.bins.get("$key").asInstanceOf[java.util.List[${ta.toString}]].asScala.toList${addComma (index, fields.length)}"""
        }

        case t if t <:< weakTypeOf[Option[_]] => t.typeArgs.head match {
          case ta if ta =:= weakTypeOf[String]
            || t =:= weakTypeOf[Int]
            || t =:= weakTypeOf[Boolean]
            || t =:= weakTypeOf[Float]
            || t =:= weakTypeOf[Double] => s"""$key = Some(rec.bins.get("$key").asInstanceOf[String])${addComma (index, fields.length)}"""

          case ta if ta.typeSymbol.isClass && ta.typeSymbol.asClass.isCaseClass =>
            val prefix = s"""rec.bins.get("$key")"""
            s"""$key = Some(${unpackMap(c)(ta, prefix)})${addComma (index, fields.length)}"""
        }

        case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass => //&& t.typeSymbol.asClass.isCaseClass =>
          // Here is value should be map (HashMap) from bin and nested type for case class.
          // There are other than case classes not available here
          val prefix = s"""rec.bins.get("$key")"""
          s"""$key = ${unpackMap(c)(t, prefix)}${addComma (index, fields.length)}"""

        // Custom classes
        case t if contains(c)(t) =>
          val name = t.typeSymbol.annotations.filter(_.tree.tpe =:= weakTypeOf[AeMacroCodeGen]).head.tree.children.tail(1) match {
            case Literal(Constant(value)) => value.asInstanceOf[String]
          }
          String.format(name, s"""rec.bins.get("$key")""")

        case _ =>  throw new Exception(s"Error compiling macro - wrong type for field (toValue): $tpeSt.$key:  ${returnType.toString} ")
      })
    } + "\n)"

    val toValue = q"""${c.parse(toValueCode)}"""

    val res = c.Expr[AerospikeCaseClassWrapper[T]] {
      q"""
      import com.aerospike.client.{Key, Value, Bin, Record}
      import com.aerospike.client.Value._
      import scala.collection.JavaConverters._
      new macros.AerospikeCaseClassWrapper[$tpe] {
        override def apply(v: $tpe): Seq[Bin] = Seq(..$values)
        override def toValue(rec: Record) = $toValue
      }
     """
    }

    res
  }

  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  private def unpackMap (c: blackbox.Context)(tpe: c.universe.Type, prefix: String): String = {
    import c.universe._

    val fields = getFields(c)(tpe)

    fields.zipWithIndex.foldLeft(s"$tpe (") { case (code, (field, index)) =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature.resultType
      val mapGet = ".asInstanceOf[java.util.Map[String, Object]]"
      val mapPref = StringContext("", mapGet)

      code + (returnType match {
        case t if t =:= weakTypeOf[String]
          || t =:= weakTypeOf[Int]
          || t =:= weakTypeOf[Boolean]
          || t =:= weakTypeOf[Float]
          || t =:= weakTypeOf[Double]
          || t =:= weakTypeOf[Array[Byte]]  =>
          s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[$returnType]${addComma (index, fields.length)}"""

        case t if t =:= weakTypeOf[Option[String]] =>
          s"""$key = Some(${mapPref.s(prefix)}.get("$key").asInstanceOf[String])${addComma (index, fields.length)}"""

        case t if t =:= weakTypeOf[List[String]] => s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[java.util.List[String]].asScala.toList${addComma (index, fields.length)}"""
        case t if t =:= weakTypeOf[List[Int]] => s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[java.util.List[Int]].asScala.toList${addComma (index, fields.length)}"""
        case t if t =:= weakTypeOf[List[Float]] => s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[java.util.List[Float]].asScala.toList${addComma (index, fields.length)}"""
        case t if t =:= weakTypeOf[List[Boolean]] => s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[java.util.List[Boolean]].asScala.toList${addComma (index, fields.length)}"""
        case t if t =:= weakTypeOf[List[Double]] => s"""$key = ${mapPref.s(prefix)}.get("$key").asInstanceOf[java.util.List[Double]].asScala.toList${addComma (index, fields.length)}"""

        // Case class
        case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass =>
          // Here is value should be map (HashMap) from bin and nested type for case class.
          // There are other than case classes not available here
          val pref = s"""${mapPref.s(prefix)}.get("$key")"""
          s"""$key = ${unpackMap(c)(returnType, pref)}${addComma (index, fields.length)}"""

        // Custom classes
        case t if contains(c)(t) =>
          val name = t.typeSymbol.annotations.filter(_.tree.tpe =:= weakTypeOf[AeMacroCodeGen]).head.tree.children.tail(1) match {
            case Literal(Constant(value)) => value.asInstanceOf[String]
          }
          String.format(name, s"""${mapPref.s(prefix)}.get("$key")""")

        case _ => throw new Exception(s"Error compiling macro - wrong type for field: $tpe.$key: ${returnType.toString}")
      })
    } + ")"
  }

  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  private def pickValue(c: blackbox.Context)(tpe: c.universe.Type, prefix: String): String = {
    import c.universe._

    val tpeSt = tpe.toString

    // Collect all fields in case class constructor (should be only one)
    val fields = getFields(c)(tpe)

    val mapValues = fields.map { field =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature.resultType

      returnType match {
        case x if x =:= weakTypeOf[String] => s""""$key" -> new StringValue($prefix.$key)"""
        case x if x =:= weakTypeOf[Int] => s""""$key" -> new IntegerValue($prefix.$key)"""
        case x if x =:= weakTypeOf[Boolean] => s""""$key" -> new BooleanValue($prefix.$key)"""
        case t if t =:= weakTypeOf[Float] => s""""$key" -> new FloatValue($prefix.$key)"""
        case t if t =:= weakTypeOf[Double] => s""""$key" -> new DoubleValue($prefix.$key)"""
        case t if t =:= weakTypeOf[Array[Byte]] => s""""$key" -> new BytesValue($prefix.$key)"""

        // Optional values
        case t if t =:= weakTypeOf[Option[String]] =>
          s""""$key" -> ($prefix.$key match {
              case Some(z) => new StringValue(z)
              case _ => null
            })"""

        // List values
        case t if t =:= weakTypeOf[List[String]]
              || t =:= weakTypeOf[List[Int]]
              || t =:= weakTypeOf[List[Float]]
              || t =:= weakTypeOf[List[Boolean]]
              || t =:= weakTypeOf[List[Double]] => s""""$key" -> new ListValue($prefix.$key.asJava)"""

        // Case classes
        case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass => s""""$key" -> Map(""" + pickValue(c)(t, s"$prefix.$key") + ").asJava"

        // Java class
        case t if t.typeSymbol.isClass && t.typeSymbol.isJava => s""""$key" -> Map(""" + pickValue(c)(t, s"$prefix.$key") + ").asJava"

        // Custom classes
        case t if contains(c)(t) =>
          val name = t.typeSymbol.annotations.filter(_.tree.tpe =:= weakTypeOf[AeMacroCodeGen]).head.tree.children.tail.head match {
            case Literal(Constant(value)) => value.asInstanceOf[String]
          }
          String.format (name, key, s"$prefix..$key")

        case _ => throw new Exception(s"Error compiling macro - wrong type for field: $tpeSt.$key: ${returnType.toString}")
      }
    }.mkString(",\n")

    mapValues
  }
}
