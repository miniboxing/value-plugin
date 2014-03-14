package valium.plugin
package transform
package inject

trait ValiumInjectTreeTransformer {
  this: ValiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  class TreeInjector(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case tree @ ValDef(mods, name, tpt, rhs) if afterInject(tree.symbol.info.isUnboxedValiumRef) =>
        treeCopy.ValDef(tree, mods, name, tpt.toUnboxedValiumRef, super.transform(rhs))
      case tree @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if !tree.symbol.isConstructor && afterInject(tree.symbol.info.finalResultType.isUnboxedValiumRef) =>
        treeCopy.DefDef(tree, mods, name, tparams, super.transformValDefss(vparamss), tpt.toUnboxedValiumRef, super.transform(rhs))
      case _ =>
        super.transform(tree)
    }
  }
}