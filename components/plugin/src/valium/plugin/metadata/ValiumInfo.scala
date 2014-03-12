package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait ValiumInfo {
  this: ValiumHelper =>

  import global._

  implicit class RichTree(tree: Tree) {
    def isValue = tree.isTerm && tree.tpe.isValue
    def toValue = { assert(tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toValue) setOriginal tree }
  }

  implicit class RichSymbol(sym: Symbol) {
    def isValium = sym != null && sym.hasAnnotation(ValiumClass)
    def isSingleFieldValium = sym.isValium && sym.primaryConstructor.paramss.flatten.length == 1
    def isMultiFieldValium = sym.isValium && sym.primaryConstructor.paramss.flatten.length > 1
  }

  implicit class RichType(tpe: Type) {
    def isValiumRef = tpe != null && tpe.dealiasWiden.typeSymbol.isValium
    def isSingleFieldValiumRef = tpe != null && tpe.dealiasWiden.typeSymbol.isSingleFieldValium
    def isMultiFieldValiumRef = tpe != null && tpe.dealiasWiden.typeSymbol.isMultiFieldValium
    def isValue = tpe != null && tpe.dealiasWiden.hasAnnotation(ValueClass)
    def toValue: Type = tpe match {
      case MethodType(params, restpe) => MethodType(params, restpe.toValue)
      case NullaryMethodType(restpe)  => NullaryMethodType(restpe.toValue)
      case PolyType(tparams, tpe)     => PolyType(tparams, tpe.toValue)
      case tpe                        => tpe.withAnnotation(AnnotationInfo marker ValueClass.tpe)
    }
  }
}