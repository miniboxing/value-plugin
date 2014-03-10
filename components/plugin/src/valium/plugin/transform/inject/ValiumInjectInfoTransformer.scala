package valium.plugin
package transform
package inject

import scala.tools.nsc.transform.InfoTransform

trait ValiumInjectInfoTransformer extends InfoTransform {
  self: ValiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // need to transform:
    // 1) def foo(x: C) = ??? => def foo(x: C @value) = ???
    // 2) val x: C = ???      => val x: C @value = ???
    // 3) def foo: C = ???    => def foo: C @value = ??? (only for 1-arg value classes)
    def logTransform(tpe1: Type): Type = {
      if (logValium) println(s"[valium-inject] $sym: $tpe -> $tpe1")
      tpe1
    }
    if (sym.isTerm && sym.isParameter && tpe.isValiumClassRef) logTransform(tpe.toValue)
    else if (sym.isTerm && !sym.isMethod && tpe.isValiumClassRef) logTransform(tpe.toValue)
    else if (sym.isMethod && !sym.isConstructor && tpe.finalResultType.isSingleValiumClassRef) logTransform(tpe.toValue)
    else tpe
  }
}
