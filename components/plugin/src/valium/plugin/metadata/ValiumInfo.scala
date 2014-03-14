package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions

trait ValiumInfo {
  this: ValiumHelper =>

  import global._
  import treeInfo._

  implicit class RichTree(tree: Tree) {
    def valiumFields = tree.tpe.valiumFields
    def isBoxedValiumRef = tree.tpe.isBoxedValiumRef
    def isUnboxedValiumRef = tree.tpe.isUnboxedValiumRef
    def toUnboxedValiumRef = { assert(tree.isType && tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toUnboxedValiumRef) setOriginal tree }
    def toBoxedValiumRef = { assert(tree.isType && tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toBoxedValiumRef) setOriginal tree }
    def isInjected = { assert(tree.symbol != null && tree.symbol != NoSymbol, (tree, tree.symbol)); tree.symbol.isInjected }
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

  implicit def nme2valiumnme(nme: global.nme.type): valiumnme.type = valiumnme
  object valiumnme {
    private def gensym(prefix: String) = TermName(prefix + globalFreshNameCreator.newName(""))
    def paramExplode(p: Symbol, f: Symbol): TermName = gensym(p.name + f.name.toString +"$p")
    def valuePrecompute(v: Symbol): TermName = gensym(v.name + "$v")
    def valueExplode(v: Symbol, f: Symbol): TermName = gensym(v.name + f.name.toString + "$v")
    def argPrecompute(p: Symbol): TermName = gensym("$f")
    def argExplode(p: Symbol, f: Symbol): TermName = gensym(f.name.toString + "$f")
    def assignPrecompute(): TermName = gensym("$a")
  }

  object Vu { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length != 0) }
  object VSu { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length == 1) }
  object VMu { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length > 1) }
  object A { def unapply(tree: Tree): Boolean = Vu.unapply(tree).isDefined && isA(tree) }
  object AS { def unapply(tree: Tree): Boolean = A.unapply(tree) && tree.valiumFields.length == 1 }
  object AM { def unapply(tree: Tree): Boolean = A.unapply(tree) && tree.valiumFields.length > 1 }
  object B { def unapply(tree: Tree): Boolean = Vu.unapply(tree).isDefined && !A.unapply(tree) }
  object BS { def unapply(tree: Tree): Boolean = B.unapply(tree) && tree.valiumFields.length == 1 }
  object BM { def unapply(tree: Tree): Boolean = B.unapply(tree) && tree.valiumFields.length > 1 }
  object C { def unapply(tree: Tree): Boolean = A.unapply(tree) || B.unapply(tree) }
  object CS { def unapply(tree: Tree): Boolean = C.unapply(tree) && tree.valiumFields.length == 1 }
  object CM { def unapply(tree: Tree): Boolean = C.unapply(tree) && tree.valiumFields.length > 1 }

  def isA(tree: Tree): Boolean = tree match {
    case Ident(_) => true
    case Select(qual, _) => qual.symbol.isStable
    case Apply(_, Nil) if tree.symbol.isGetter => true
    case Apply(_, arg :: Nil) if tree.symbol.isInjected => true
    case _ => false
  }

  def box2unbox(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(box2unbox), List(tree)) setType tree.tpe.toUnboxedValiumRef)
  def box2unbox(sym: Symbol): Tree = box2unbox(gen.mkAttributedRef(sym))
  def unbox2box(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(unbox2box), List(tree)) setType tree.tpe.toBoxedValiumRef)
  def unbox2box(tree: Tree, field: Symbol): Tree = atPos(tree.pos)(gen.mkAttributedSelect(unbox2box(tree), field) setType unbox2box(tree).tpe.memberInfo(field).finalResultType)
  def unbox2box(sym: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym))
  def unbox2box(sym: Symbol, field: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym), field)
}