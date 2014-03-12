package valium.plugin
package transform
package addext

import scala.tools.nsc.transform.TypingTransformers

trait ValiumConvertTreeTransformer extends TypingTransformers {
  this: ValiumConvertPhase =>

  import global._
  import definitions._

  class TreeConverter(unit: CompilationUnit) extends TypingTransformer(unit) {
    // need to transform:
    // notation:
    //    x => something that has type C @value
    //    expr => anything
    //    foo => method that has one or more of its parameters looking like C @value
    //    lhs => an lvalue
    // 01) def foo(x: C @value) = expr => def foo(x$x: Int, x$y: Int) = expr
    // 02) val x: C @value = expr      => val x$x: Int = expr.x; val x$y: Int = expr.y
    // 03) def foo: C @value = expr    => def foo: Int = expr.x (only for 1-arg value classes)
    // 04) foo(expr)                   => foo(expr.x, expr.y) // this can recursively trigger one of the subsequent rules
    // 05) lhs = x                     => lhs.x = x.x; lhs.y = x.y // this can recursively trigger one of the subsequent rules
    // 06) return x                    => return x.x // can only happen for 1-field valium classes
    // 07) unbox2box(x).field          => x$field
    // 08) unbox2box(x)                => new C(x$x, x$y)
    // 09) box2unbox(expr).field       => expr.field
    // 10) box2unbox(expr)             => ??? TODO: can this ever happen?
    // NOTE: when doing expr.x, we need to stabilize expr first
    override def transform(tree: Tree): Tree = ???
  }
}