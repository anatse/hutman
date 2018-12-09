package macros

import scala.reflect.macros.blackbox

object MacroCommon {
  @inline private[macros] def addComma (index: Int, size: Int) = if (index < size - 1) ", " else ""

  @inline private[macros] def getFields (c: blackbox.Context)(tpe: c.universe.Type) = {
    import c.universe._

    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head
  }

  @inline private[macros] def contains (c: blackbox.Context)(tpe: c.universe.Type) = {
    import c.universe._
    val anns = tpe.typeSymbol.annotations
    anns.exists(_.tree.tpe =:= weakTypeOf[AeMacroCodeGen])
  }
}
