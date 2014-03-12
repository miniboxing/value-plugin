package valium.plugin
package transform
package convert

import scala.tools.nsc.transform.InfoTransform

trait ValiumConvertInfoTransformer extends InfoTransform {
  self: ValiumConvertPhase =>

  import global._
  import definitions._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // need to transform:
    // 1) def foo(x: C @value) = ??? => def foo(x$x: Int, x$y: Int) = ???
    // 2) val x: C @value = ???      => val x$x: Int = ???; val x$y: Int = ???
    // 3) def foo: C @value = ???    => def foo: Int = ??? (only for 1-arg value classes)
    // 4) unbox2box(x).field         => x$field
    tpe
  }
}
