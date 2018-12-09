package macros

import com.arangodb.entity.BaseDocument

import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox
import scala.language.experimental.macros
import MacroCommon._

/**
  * Case class contains definition of BaseDocument and additional parameters converted to T type
  * @param base BaseDocument
  * @param cc instance of T class constructed from additional parameters
  * @tparam T type of contained case class
  */
case class ArangoDoc[T] (base: BaseDocument, cc: T)

/**
  * Trait represents functions of convert codec for specified type. This trait used in compilation time to create codecs
  * @tparam T type to convert from/to [[com.arangodb.entity.BaseDocument]]
  */
trait ArangoMapper[T] {
  /**
    * Function served to coping one base document to another without additional parameters. Used during convert base document
    * to [[macros.ArangoDoc]]
    * @param bdoc document to make copy from
    * @return  copy of the given document without additional attributes
    */
  def copyDoc (bdoc:  BaseDocument): BaseDocument = {
    if (bdoc == null) {
      null
    } else {
      val res = new BaseDocument(bdoc.getKey)
      res.setRevision(bdoc.getRevision)
      res.setId(bdoc.getId)
      res
    }
  }

  def apply(t: T): BaseDocument
  def fromBase(t: BaseDocument): ArangoDoc[T]
}

/**
  * Object provides macros to make codec for case classes which allowed to manipulate them as database objects.
  * Used in functions provided by ArangoDB scala driver
  * @example
  * {{{
  *   case class Test (testAttr:String, ...)
  *
  *   import ArangoMacros._
  *
  *   // Create codec for Test case class
  *   implicit val aw:ArangoMapper[Test] = convertDoc[Test]
  *
  *   ...
  *
  *   val test = Test("Hello", ...)
  *
  *   // Saving in arangodb database. Will store testAttr:Hello
  *   db.collection("users").insert(test).map (res => ...)
  *
  *   // Getting from database by key. Will print Hello
  *   usersCol.getDocument[Test]("56366").map (found => println (found.testAttr))
  * }}}
  */
object ArangoMacros {
  def convertDoc[T]: ArangoMapper[T] = macro convertDocImpl[T]

  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def convertDocImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[ArangoMapper[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val parMap = s"""Map(${caseClassToMap(c)(tpe, "t")}).asJava"""
    val paramsQ = q"""${c.parse(parMap)}"""

    val casclQ = q"""${c.parse(caseClassFromMap(c)(tpe, "t.getProperties"))}"""

    c.Expr[ArangoMapper[T]] { q"""
      import scala.collection.JavaConverters._
      import com.arangodb.entity.BaseDocument
      import macros.ArangoMapper
      import macros.ArangoDoc

      new ArangoMapper[$tpe] {
        def apply(t: $tpe): BaseDocument = new BaseDocument($paramsQ)
        def fromBase(t: BaseDocument): ArangoDoc[$tpe] = if (t == null) null else ArangoDoc (copyDoc(t), $casclQ)
      }
    """ }
  }

  def caseClassToMap (c: blackbox.Context)(tpe: c.universe.Type, prefix: String): String = {
    import c.universe._

    println (s"caseClassToMap-start: $tpe")
    val fields = getFields(c)(tpe)
    println (s"caseClassToMap-end: $tpe")

    fields.map { field =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature.resultType

      returnType match {
        case t if t =:= weakTypeOf[String]
          || t <:< weakTypeOf[AnyVal]
          || t =:= weakTypeOf[Boolean] => s""""$key" -> $prefix.$key.asInstanceOf[AnyRef]"""

        case t if t <:< weakTypeOf[List[_]] =>
          t.typeArgs.head match {
            case t if t =:= weakTypeOf[String]
              || t <:< weakTypeOf[AnyVal]
              || t =:= weakTypeOf[Boolean] =>
              s""""$key" -> $prefix.$key.asJava"""

            case ut => throw new RuntimeException(s"Unknown list argument type for arango macros ${ut.toString}")
          }

        case t if t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass => s""""$key" -> Map(${caseClassToMap(c)(t, s"$prefix.$key")}).asJava"""

        // Optional types
        case t if t <:< weakTypeOf[Option[_]] =>
          t.typeArgs.head match {
            case ta if ta =:= weakTypeOf[String]
              || ta <:< weakTypeOf[AnyVal]
              || ta =:= weakTypeOf[Boolean] =>
                s"""$prefix.$key match {
                   |case Some(v) => ("$key" -> v.asInstanceOf[AnyRef])
                   |case _ => ("$key" -> null)
                   |}""".stripMargin

            case ta if ta.typeSymbol.isClass && ta.typeSymbol.asClass.isCaseClass =>
              s"""$prefix.$key match {
                 |case Some(v) => "$key" -> Map(${caseClassToMap(c)(ta, "v")}).asJava
                 |case _ => ("$key" -> null)
                 |}""".stripMargin
          }

        case ut => throw new RuntimeException(s"Unknown type for arango macros ${ut.toString}")
      }
    }.mkString(",\n")
  }

  def caseClassFromMap (c: blackbox.Context)(tpe: c.universe.Type, prefix: String): String = {
    import c.universe._

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

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
          s"""$key = ${caseClassFromMap(c)(returnType, pref)}${addComma (index, fields.length)}"""

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
}
