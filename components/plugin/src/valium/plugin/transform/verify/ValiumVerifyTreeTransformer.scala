package valium.plugin
package transform
package verify

import scala.reflect.internal.Flags._

trait ValiumVerifyTreeTransformer {
  this: ValiumVerifyPhase =>

  import global._
  import definitions._
  import helper._

  class TreeVerifier(unit: CompilationUnit) extends Traverser {
    override def traverse(tree: Tree): Unit = tree match {
      case ClassDef(_, _, tparams, Template(_, _, stats)) if tree.symbol.isValiumClass =>
        if (tree.symbol.isAbstract) unit.error(tree.pos, "`abstract' modifier cannot be used with valium classes")
        tree.symbol.setFlag(FINAL)
        val constrParamAccessors = tree.symbol.constrParamAccessors.map(field => (field, field.getterIn(field.owner), field.setter(field.owner)))
        constrParamAccessors collect { case (field, getter, _) if getter == NoSymbol || !getter.isPublic => unit.error(field.pos, "there can only be public fields in valium classes") }
        constrParamAccessors collect { case (field, _, setter) if setter != NoSymbol => unit.error(field.pos, "there can only be immutable fields in valium classes") }
        constrParamAccessors collect { case (field, _, _) if field.info.typeSymbol.isValiumClass => unit.error(field.pos, "there can only be non-valiumclass fields in valium classes") }
        // TODO: this is to avoid dealing with types dependent on valium classes
        // those can be supported in valium-convert by just replacing p.T's to their upper bounds
        // (that's valid, because valium classes are final, and because typechecker has already checked that path-dependent types are ok)
        // however that would be tedious in the sense that we need to make these replacements everywhere - in trees, in symbol signatures, etc
        stats collect { case tdef: TypeDef => unit.error(tdef.pos, "type members can't be defined in valium classes") }
        stats collect { case vdef: ValDef if !vdef.symbol.isParamAccessor => unit.error(vdef.pos, "value members can't be defined in valium classes") }
        super.traverse(tree)
      case _: MemberDef if tree.symbol.isValiumClass =>
        unit.error(tree.pos, "only classes (not traits) are allowed to be @valium")
        super.traverse(tree)
      case _: ImplDef if tree.symbol.info.parents.exists(_.typeSymbol.isValiumClass) =>
        unit.error(tree.pos, "can't inherit from a valium class")
        super.traverse(tree)
      case _ =>
        // TODO: need to ban p.type for valium classes
        super.traverse(tree)
    }
  }
}