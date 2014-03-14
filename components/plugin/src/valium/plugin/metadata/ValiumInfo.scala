package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions

trait ValiumInfo {
  this: ValiumHelper =>

  import global._
  import treeInfo._

  implicit class RichTree(tree: Tree) {
    def valiumFields = tree.tpe.valiumFields
    def valiumField = tree.tpe.valiumField
    def isBoxedValiumRef = tree.tpe.isBoxedValiumRef
    def isUnboxedValiumRef = tree.tpe.isUnboxedValiumRef
    def toUnboxedValiumRef = { assert(tree.isType && tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toUnboxedValiumRef) setOriginal tree }
    def toBoxedValiumRef = { assert(tree.isType && tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toBoxedValiumRef) setOriginal tree }
    def toValiumField = { assert(tree.isType && tree.tpe != null, (tree, tree.tpe)); TypeTree(tree.tpe.toValiumField) setOriginal tree }
    def isInjected = { assert(tree.symbol != null && tree.symbol != NoSymbol, (tree, tree.symbol)); tree.symbol.isInjected }
  }

  implicit class RichSymbol(sym: Symbol) {
    def isValiumClass = sym != null && sym.hasAnnotation(ValiumClass)
    def valiumFields = if (sym.isValiumClass || sym.isBoxedValiumRef || sym.isUnboxedValiumRef) sym.info.members.sorted.filter(sym => sym.isMethod && sym.isParamAccessor).toList else Nil
    def valiumField = valiumFields match { case f :: Nil => f; case _ => throw new Exception(sym.toString) }
    def isBoxedValiumRef = sym != null && sym.info.isBoxedValiumRef
    def isUnboxedValiumRef = sym != null && sym.info.isUnboxedValiumRef
    def isInjected = sym == box2unbox || sym == unbox2box
    def registerExploded(exploded: Symbol) = sym.updateAttachment(ExplodedSymbolsAttachment(sym.explodedSymbols :+ exploded))
    def explodedSymbols = sym.attachments.get[ExplodedSymbolsAttachment].map(_.syms).getOrElse(Nil)
  }

  case class ExplodedSymbolsAttachment(syms: List[Symbol])

  implicit class RichType(tpe: Type) {
    def valiumFields = if (tpe != null) tpe.dealiasWiden.typeSymbol.valiumFields else Nil
    def valiumField = if (tpe != null) tpe.dealiasWiden.typeSymbol.valiumField else throw new Exception("")
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
    def toValiumField: Type = tpe.valiumFields match {
      case f :: Nil => tpe.memberInfo(f).finalResultType
      case _ => throw new Exception(tpe.toString)
    }
  }

  implicit def nme2valiumnme(nme: global.nme.type): valiumnme.type = valiumnme
  object valiumnme {
    private def gensym(prefix: String) = TermName(prefix + globalFreshNameCreator.newName(""))
    def paramExplode(p: Symbol, f: Symbol): TermName = paramExplode(p, f.name)
    def paramExplode(p: Symbol, n: Name): TermName = gensym(p.name + n.toString + "$")
    def isParamExplode(p: Symbol, n: Name, candidate: Symbol): Boolean = candidate.name.startsWith(p.name + n.toString)
    def valueExplode(v: Symbol, f: Symbol): TermName = valueExplode(v, f.name)
    def valueExplode(v: Symbol, n: Name): TermName = gensym(v.name + n.toString + "$")
    def isValueExplode(v: Symbol, n: Name, candidate: Symbol): Boolean = candidate.name.startsWith(v.name + n.toString)
    def argPrecompute(p: Symbol): TermName = gensym("$")
    def argExplode(p: Symbol, f: Symbol): TermName = gensym(f.name.toString + "$")
    def assignPrecompute(): TermName = gensym("$")
  }

  implicit def gen2valiumgen(gen: global.gen.type): valiumgen.type = valiumgen
  object valiumgen {
    def mkExplodedRef(e: Tree, a: Symbol, x: Name): Tree = {
      // can's just call nme.valueExplode(a, x) because of gensym!
      val ax = a.explodedSymbols.find(cand => nme.isParamExplode(a, x, cand) || nme.isValueExplode(a, x, cand)).getOrElse(throw new Exception(s"$e, $a, $x"))
      RefTree(e, ax.name) setSymbol ax
    }
  }
  object Eax {
    def apply(e: Tree, a: Symbol, x: Symbol): Tree = apply(e, a, x.name)
    def apply(e: Tree, a: Symbol, x: Name): Tree = gen.mkExplodedRef(e, a, x)
  }

  object Vu {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length != 0)
    def unapply(vparamss: List[List[ValDef]]): Boolean = vparamss.flatten.exists(p => Vu.unapply(p.tpt).isDefined)
  }
  object VSu { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length == 1) }
  object VMu { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length > 1) }
  object A { def unapply(tree: Tree): Option[(Tree, Symbol)] = if (Vu.unapply(tree).isDefined && isA(tree)) Some(extractQualAndSymbol(tree)) else None }
  object AS { def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).filter(_ => tree.valiumFields.length == 1) }
  object AM { def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).filter(_ => tree.valiumFields.length > 1) }
  object B { def unapply(tree: Tree): Option[(Tree, Symbol)] = if (Vu.unapply(tree).isDefined && !isA(tree)) Some(extractQualAndSymbol(tree)) else None }
  object BS { def unapply(tree: Tree): Option[(Tree, Symbol)] = B.unapply(tree).filter(_ => tree.valiumFields.length == 1) }
  object BM { def unapply(tree: Tree): Option[(Tree, Symbol)] = B.unapply(tree).filter(_ => tree.valiumFields.length > 1) }
  object C { def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).orElse(B.unapply(tree)) }
  object CS { def unapply(tree: Tree): Option[(Tree, Symbol)] = C.unapply(tree).filter(_ => tree.valiumFields.length == 1) }
  object CM { def unapply(tree: Tree): Option[(Tree, Symbol)] = C.unapply(tree).filter(_ => tree.valiumFields.length > 1) }
  object V { def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isBoxedValiumRef && fields.length != 0) }
  object ES { def unapply(tree: Tree): Option[(Tree, Symbol)] = if (V.unapply(tree).isDefined && tree.valiumFields.length == 1) Some(extractQualAndSymbol(tree)) else None }
  object EM { def unapply(tree: Tree): Option[(Tree, Symbol)] = if (V.unapply(tree).isDefined && tree.valiumFields.length > 1) Some(extractQualAndSymbol(tree)) else None }
  object Unbox2box { def unapply(tree: Tree): Option[Tree] = tree match { case Apply(_, arg :: Nil) if tree.symbol == unbox2box => Some(arg); case _ => None } }
  object Box2unbox { def unapply(tree: Tree): Option[Tree] = tree match { case Apply(_, arg :: Nil) if tree.symbol == box2unbox => Some(arg); case _ => None } }

  def isA(tree: Tree): Boolean = isC(tree) && (tree match {
    case Ident(_) => true
    case Select(This(_), _) if !tree.symbol.isMethod => true
    case Select(Super(_, _), _) if !tree.symbol.isMethod => true
    case Select(qual, _) if !tree.symbol.isMethod => qual.symbol.isStable
    case _ => false
  })

  def isB(tree: Tree): Boolean = isC(tree) && !isA(tree)

  def isC(tree: Tree): Boolean = tree.isUnboxedValiumRef

  def extractQualAndSymbol(tree: Tree) = tree match {
    case RefTree(qual, _) => (qual, tree.symbol)
    case _ => (EmptyTree, tree.symbol)
  }

  def box2unbox(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(box2unbox), List(tree)) setType tree.tpe.toUnboxedValiumRef)
  def box2unbox(sym: Symbol): Tree = box2unbox(gen.mkAttributedRef(sym))
  def unbox2box(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(unbox2box), List(tree)) setType tree.tpe.toBoxedValiumRef)
  def unbox2box(tree: Tree, field: Symbol): Tree = atPos(tree.pos)(gen.mkAttributedSelect(unbox2box(tree), field) setType unbox2box(tree).tpe.memberInfo(field).finalResultType)
  def unbox2box(sym: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym))
  def unbox2box(sym: Symbol, field: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym), field)
}