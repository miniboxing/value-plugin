package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait ValiumInfo {
  this: ValiumHelper =>

  import global._

  implicit class RichTree(tree: Tree) {
    def isValiumClassRef = tree.tpe.isValiumClassRef
    def isSingleValiumClassRef = tree.tpe.isSingleValiumClassRef
    def toValue = TypeTree(tree.tpe.toValue) setOriginal tree
  }

  implicit class RichSymbol(sym: Symbol) {
    def isValiumClass = sym != null && sym.hasAnnotation(ValiumClass)
    def isSingleValiumClass = sym.isValiumClass && sym.primaryConstructor.paramss.flatten.length == 1
  }

  implicit class RichType(tpe: Type) {
    def isValiumClassRef = tpe != null && tpe.dealiasWiden.typeSymbol.isValiumClass
    def isSingleValiumClassRef = tpe != null && tpe.dealiasWiden.typeSymbol.isSingleValiumClass
    def isValue = tpe != null && tpe.dealiasWiden.hasAnnotation(ValueClass)
    def toValue: Type = tpe match {
      case MethodType(params, restpe) => MethodType(params, restpe.toValue)
      case NullaryMethodType(restpe)  => NullaryMethodType(restpe.toValue)
      case PolyType(tparams, tpe)     => PolyType(tparams, tpe.toValue)
      case tpe                        => tpe.withAnnotation(AnnotationInfo marker ValueClass.tpe)
    }
  }
}