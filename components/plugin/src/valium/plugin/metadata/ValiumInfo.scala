package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait ValiumInfo {
  this: ValiumHelper =>

  import global._

  implicit class RichTree(tree: Tree) {
    def valiumFields = tree.tpe.valiumFields
    def isBoxedValiumRef = tree.isTerm && tree.tpe.isBoxedValiumRef
    def isUnboxedValiumRef = tree.isTerm && tree.tpe.isUnboxedValiumRef
    def toUnboxedValiumRef = { assert(tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toUnboxedValiumRef) setOriginal tree }
  }

  implicit class RichSymbol(sym: Symbol) {
    def isValiumClass = sym != null && sym.hasAnnotation(ValiumClass)
    def valiumFields = if (sym.isValiumClass) sym.info.members.filter(sym => !sym.isMethod && sym.isParamAccessor).toList else Nil
    def isBoxedValiumRef = sym != null && sym.info.isBoxedValiumRef
    def isUnboxedValiumRef = sym != null && sym.info.isUnboxedValiumRef
  }

  implicit class RichType(tpe: Type) {
    def valiumFields = tpe.dealiasWiden.typeSymbol.valiumFields
    def isBoxedValiumRef = tpe != null && tpe.dealiasWiden.typeSymbol.isValiumClass && !tpe.isUnboxedValiumRef
    def isUnboxedValiumRef = tpe != null && tpe.dealiasWiden.hasAnnotation(UnboxedAnnotationClass)
    def toUnboxedValiumRef: Type = tpe match {
      case MethodType(params, restpe) => MethodType(params, restpe.toUnboxedValiumRef)
      case NullaryMethodType(restpe)  => NullaryMethodType(restpe.toUnboxedValiumRef)
      case PolyType(tparams, tpe)     => PolyType(tparams, tpe.toUnboxedValiumRef)
      case tpe                        => tpe.withAnnotation(AnnotationInfo marker UnboxedAnnotationClass.tpe)
    }
  }
}