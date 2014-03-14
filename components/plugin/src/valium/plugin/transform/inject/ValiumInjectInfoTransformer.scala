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
    def logTransform(tpe1: Type): Type = { valiumlog(s"$sym: $tpe -> $tpe1"); tpe1 }
    if (sym.isTerm && sym.isParameter && tpe.isBoxedValiumRef)
      logTransform(tpe.toUnboxedValiumRef)
    else if (sym.isTerm && !sym.isMethod && tpe.isBoxedValiumRef)
      logTransform(tpe.toUnboxedValiumRef)
    else if (sym.isMethod && tpe.finalResultType.isBoxedValiumRef && tpe.finalResultType.valiumFields.length == 1 && !sym.isInjected)
      logTransform(tpe.toUnboxedValiumRef)
    else
      tpe
  }
}
