package valium.plugin
package transform
package inject

import scala.tools.nsc.transform.TypingTransformers

trait ValiumInjectTreeTransformer extends TypingTransformers {
  this: ValiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  class TreeInjector(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case tree @ ValDef(mods, name, tpt, rhs) if afterInject(tree.symbol.info.isValue) =>
        treeCopy.ValDef(tree, mods, name, tpt.toValue, super.transform(rhs))
      case tree @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if afterInject(tree.symbol.info.finalResultType.isValue) =>
        treeCopy.DefDef(tree, mods, name, tparams, super.transformValDefss(vparamss), tpt.toValue, super.transform(rhs))
      case _ =>
        super.transform(tree)
    }
  }
}