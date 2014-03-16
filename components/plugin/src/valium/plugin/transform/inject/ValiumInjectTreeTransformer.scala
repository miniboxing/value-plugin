package valium.plugin
package transform
package inject

trait ValiumInjectTreeTransformer {
  this: ValiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  class TreeInjector(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree)(implicit state: State) = {
      case tree @ ValDef(mods, name, tpt @ V(_), rhs) =>
        commit(treeCopy.ValDef(tree, mods, name, tpt.toUnboxedValiumRef, rhs))
      case tree @ DefDef(mods, name, tparams, vparamss, tpt @ V(_), rhs) if tree.symbol.info.finalResultType.isUnboxedValiumRef =>
        commit(treeCopy.DefDef(tree, mods, name, tparams, super.transformValDefss(vparamss), tpt.toUnboxedValiumRef, rhs))
    }
  }
}