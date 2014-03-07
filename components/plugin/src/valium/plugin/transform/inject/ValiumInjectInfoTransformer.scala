package valium.plugin
package transform
package inject

import scala.tools.nsc.transform.InfoTransform

trait ValiumInjectInfoTransformer extends InfoTransform {
  self: ValiumInjectPhase =>

  import global._
  import definitions._
  import valium._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // need to transform:
    // 1) val x: @valium C = ???
    // 2) return types for 1-param value classes
    val isValThatNeedsTransform = sym.isTerm && !sym.isMethod && tpe.typeSymbol.isValium
    if (isValThatNeedsTransform) tpe.withAnnotation(AnnotationInfo marker ValueClass.tpe)
    else {
      tpe
    }
  }
}
