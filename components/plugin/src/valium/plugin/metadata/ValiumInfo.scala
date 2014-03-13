package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait ValiumInfo {
  this: ValiumHelper =>

  import global._
  import treeInfo._

  implicit class RichTree(tree: Tree) {
    def valiumFields = tree.tpe.valiumFields
    def isBoxedValiumRef = tree.isTerm && tree.tpe.isBoxedValiumRef
    def isUnboxedValiumRef = tree.isTerm && tree.tpe.isUnboxedValiumRef
    def toUnboxedValiumRef = { assert(tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toUnboxedValiumRef) setOriginal tree }
    def toBoxedValiumRef = { assert(tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toBoxedValiumRef) setOriginal tree }
    def isInjected = { assert(tree.symbol != null && tree.symbol != NoSymbol, (tree, tree.symbol)); tree.symbol.isInjected }
    def needsPrecompute: Boolean = tree match {
      case Applied(core, _, List(List(arg))) if core.isInjected => arg.needsPrecompute
      case _ => !isExprSafeToInline(tree)
    }
  }

  implicit class RichSymbol(sym: Symbol) {
    def isValiumClass = sym != null && sym.hasAnnotation(ValiumClass)
    def valiumFields = if (sym.isValiumClass || sym.isBoxedValiumRef || sym.isUnboxedValiumRef) sym.info.members.sorted.filter(sym => sym.isMethod && sym.isParamAccessor).toList else Nil
    def isBoxedValiumRef = sym != null && sym.info.isBoxedValiumRef
    def isUnboxedValiumRef = sym != null && sym.info.isUnboxedValiumRef
    def isInjected = sym == box2unbox || sym == unbox2box
  }

  implicit class RichType(tpe: Type) {
    def valiumFields = tpe.dealiasWiden.typeSymbol.valiumFields
    def isBoxedValiumRef = tpe != null && tpe.dealiasWiden.typeSymbol.isValiumClass && !tpe.isUnboxedValiumRef
    def isUnboxedValiumRef = tpe != null && tpe.dealiasWiden.hasAnnotation(UnboxedClass)
    def toUnboxedValiumRef: Type = tpe match {
      case MethodType(params, restpe) => MethodType(params, restpe.toUnboxedValiumRef)
      case NullaryMethodType(restpe)  => NullaryMethodType(restpe.toUnboxedValiumRef)
      case PolyType(tparams, tpe)     => PolyType(tparams, tpe.toUnboxedValiumRef)
      case tpe                        => tpe.withAnnotation(AnnotationInfo marker UnboxedClass.tpe)
    }
    def toBoxedValiumRef: Type = tpe match {
      case MethodType(params, restpe) => MethodType(params, restpe.toBoxedValiumRef)
      case NullaryMethodType(restpe)  => NullaryMethodType(restpe.toBoxedValiumRef)
      case PolyType(tparams, tpe)     => PolyType(tparams, tpe.toBoxedValiumRef)
      case tpe                        => tpe.removeAnnotation(UnboxedClass)
    }
  }
}