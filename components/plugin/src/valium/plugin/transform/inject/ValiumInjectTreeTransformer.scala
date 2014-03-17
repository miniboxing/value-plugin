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
      case tree if tree.symbol.isValiumBridge =>
        fallback()
      case tree @ ValDef(mods, name, tpt @ V(_), rhs) =>
        commit(treeCopy.ValDef(tree, mods, name, tpt.toUnboxedValiumRef, rhs))
      case tree @ DefDef(mods, name, tparams, vparamss, tpt @ VS(_), rhs) =>
        commit(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt.toUnboxedValiumRef, rhs))
      case tree @ DefDef(_, _, _, _, _, rhs) if tree.symbol.info.params.exists(_.isUnboxedValiumRef) || tree.symbol.info.finalResultType.isUnboxedValiumRef =>
        val hasBridges = tree.symbol.owner.info.decl(tree.symbol.name).alternatives.exists(_.isValiumBridge)
        lazy val bridges = beforeInject(tree.symbol.allOverriddenSymbols.flatMap(base => {
          val owned = tree.symbol.owner.info.memberInfo(base)
          val params1 = map3(tree.symbol.info.params, base.info.params, owned.params.map(_.cloneSymbol))((our, their, owned) => {
            // TODO: does this heuristic actually correspond to the real state of things?
            val needsBridging = afterInject(our.isUnboxedValiumRef ^ their.isUnboxedValiumRef)
            if (needsBridging) { valiumlog("PARAMBRIDGE: " + owned); owned.markValiumBridge } else owned
          })
          if (!params1.exists(_.isValiumBridge)) None
          else Some({
            val bridge = tree.symbol.cloneSymbol
            // TODO: this MethodType is obviously incomplete in polymorphic context
            bridge.setInfo(MethodType(params1.map(_.cloneSymbol(bridge)), owned.finalResultType))
            valiumlog("DEFBRIDGE: " + owned)
            bridge.markValiumBridge
          })
        }))
        if (hasBridges || bridges.isEmpty) fallback()
        else {
          val bridgeDefs = bridges.map(bridge => {
            tree.symbol.owner.info.decls.enter(bridge)
            val bridgeRhs = beforeInject(localTyper.typed(Apply(Ident(tree.symbol), bridge.info.params.map(Ident))))
            val bridgeDef = newDefDef(bridge, bridgeRhs)() setType NoType
            mmap(bridgeDef.vparamss)(_ setType NoType)
            bridgeDef
          })
          super.transform(tree) +: bridgeDefs
        }
    }
  }
}