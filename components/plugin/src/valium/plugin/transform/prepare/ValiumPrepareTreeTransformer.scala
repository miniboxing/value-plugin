package valium.plugin
package transform
package verify

import scala.reflect.internal.Flags._

trait ValiumPrepareTreeTransformer {
  this: ValiumPreparePhase =>

  import global._
  import definitions._
  import helper._

  class TreePreparer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      // remove redundant asInstanceOf-s introduced by tailcalls
      case AsInstanceOf(expr, tpe) if expr.tpe =:= tpe.tpe && tpe.tpe.typeSymbol.isValiumClass =>
        valiumlog("removed redundant asInstanceOf:")
        valiumlog("  tree: " + tree)
        valiumlog("  expr: " + tree)
        valiumlog("  tree.tpe: " + tree.tpe)
        valiumlog("  expr.tpe: " + expr.tpe)
        valiumlog("  tpe.tpe: " + tpe.tpe)
        expr
      case _ =>
        super.transform(tree)
    }
  }
}