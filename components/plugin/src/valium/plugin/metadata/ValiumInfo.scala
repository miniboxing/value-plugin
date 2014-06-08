package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.internal.Flags._

trait ValiumInfo {
  this: ValiumHelper =>

  import global._
  import treeInfo._
  import definitions._

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
    def valiumFields = if (sym.isValiumClass || sym.isBoxedValiumRef || sym.isUnboxedValiumRef) sym.info.members.sorted.filter(sym => sym.isMethod && sym.isParamAccessor && sym.isGetter).toList else Nil
    def valiumField = valiumFields match { case f :: Nil => f; case _ => throw new Exception(sym.toString) }
    def isBoxedValiumRef = sym != null && sym.info.isBoxedValiumRef
    def isUnboxedValiumRef = sym != null && sym.info.isUnboxedValiumRef
    def isInjected = sym == box2unbox || sym == unbox2box
    def registerExploded(x: Symbol, exploded: Symbol) = sym.updateAttachment(ExplodedSymbolsAttachment(sym.explodedSymbols + (x.name -> exploded)))
    def explodedSymbols = sym.attachments.get[ExplodedSymbolsAttachment].map(_.syms).getOrElse(Map())
    def isValiumBridge = sym != null && sym.hasAnnotation(BridgeClass)
    def markValiumBridge = { if (sym != null) sym.addAnnotation(AnnotationInfo.marker(BridgeClass.tpe)); sym }
    def isValiumMethodWithExtension = sym.isMethod && sym.owner.isValiumClass && !sym.isParamAccessor && !sym.isConstructor && !sym.hasFlag(SUPERACCESSOR) && !sym.isMacro
  }

  case class ExplodedSymbolsAttachment(syms: Map[Name, Symbol])

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
    def paramExplode(p: Symbol, n: Name): TermName = TermName(p.name + "$" + n.toString)
    def valueExplode(v: Symbol, f: Symbol): TermName = valueExplode(v, f.name)
    def valueExplode(v: Symbol, n: Name): TermName = {
      val mangled = TermName(v.name.dropLocal + "$" + n.toString)
      if (nme.isLocalName(v.name)) mangled.localName else mangled
    }
    def valuePrecompute(v: Symbol): TermName = gensym("$")
    def argPrecompute(p: Symbol): TermName = gensym("$")
    def argExplode(p: Symbol, f: Symbol): TermName = TermName("arg$" + p.name + "$" + f.name)
    def assignPrecompute(): TermName = gensym("$")
  }

  implicit def gen2valiumgen(gen: global.gen.type): valiumgen.type = valiumgen
  object valiumgen {
    def mkExplodedRef(e: Tree, a: Symbol, x: Name): Tree = {
      // can's just call nme.valueExplode(a, x) because of gensym!
      def fail() = throw new Exception(s"can't resolve $e.${a.name}$$$x (exploded symbols of $a (flags = ${a.flags}) are ${a.explodedSymbols})")
      val ax = a.explodedSymbols.get(x).getOrElse(fail())
      RefTree(e, ax.name) setSymbol ax
    }
  }
  object Eax {
    def apply(e: Tree, a: Symbol, x: Symbol): Tree = apply(e, a, x.name)
    def apply(e: Tree, a: Symbol, x: Name): Tree = gen.mkExplodedRef(e, a, x)
  }

  object V {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isBoxedValiumRef && fields.length != 0)
  }

  object Vu {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length != 0)
  }

  object Vus {
    def unapply(vparams: List[Ident]): Boolean = vparams.exists(p => Vu.unapply(p).isDefined)
  }

  object Vuss {
    def unapply(vparamss: List[List[ValDef]]): Boolean = vparamss.flatten.exists(p => Vu.unapply(p.tpt).isDefined)
  }

  object VS {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isBoxedValiumRef && fields.length == 1)
  }

  object VSu {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length == 1)
  }

  object VMu {
    def unapply(tree: Tree): Option[List[Symbol]] = Some(tree.valiumFields).filter(fields => tree.isUnboxedValiumRef && fields.length > 1)
  }

  // during the coercion phase, types may be inconsistent, thus
  // checking tree.tpe.isUnboxed in isC is too restrictive.
  def looksLikeA(tree: Tree): Boolean = tree match {
    case This(_) => true
    case Ident(_) => true
    case Select(This(_), _) if !tree.symbol.isMethod => true
    case Select(Super(_, _), _) if !tree.symbol.isMethod => true
    case Select(qual, _) if !tree.symbol.isMethod => qual.symbol.isStable
    case _ => false
  }

  def isA(tree: Tree): Boolean = isC(tree) && looksLikeA(tree)

  def isB(tree: Tree): Boolean = isC(tree) && !isA(tree)

  def isC(tree: Tree): Boolean = tree.isUnboxedValiumRef

  def extractQualAndSymbol(tree: Tree) = tree match {
    case RefTree(qual, _) => (qual, tree.symbol)
    case _ => (EmptyTree, tree.symbol)
  }

  object A {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = {
      if (Vu.unapply(tree).isDefined && isA(tree)) Some(extractQualAndSymbol(tree))
      else None
    }
  }

  object AS {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).filter(_ => tree.valiumFields.length == 1)
  }

  object AM {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).filter(_ => tree.valiumFields.length > 1)
  }

  object B {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = {
      if (Vu.unapply(tree).isDefined && !isA(tree)) Some(extractQualAndSymbol(tree))
      else None
    }
  }

  object BS {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = B.unapply(tree).filter(_ => tree.valiumFields.length == 1)
  }

  object BM {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = B.unapply(tree).filter(_ => tree.valiumFields.length > 1)
  }

  object C {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = A.unapply(tree).orElse(B.unapply(tree))
  }

  object CS {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = C.unapply(tree).filter(_ => tree.valiumFields.length == 1)
  }

  object CM {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = C.unapply(tree).filter(_ => tree.valiumFields.length > 1)
  }

  object ES {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = {
      if (V.unapply(tree).isDefined && tree.valiumFields.length == 1) Some(extractQualAndSymbol(tree))
      else None
    }
  }

  object EM {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = {
      if (V.unapply(tree).isDefined && tree.valiumFields.length > 1) Some(extractQualAndSymbol(tree))
      else None
    }
  }

  object U {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case Apply(core, args) if !core.symbol.isInjected && args.exists(_.isUnboxedValiumRef) => Some((core, args))
      case _ => None
    }
  }

  object Unbox2box {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == unbox2box => Some(arg)
      case _ => None
    }
  }

  object Box2unbox {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == box2unbox => Some(arg)
      case _ => None
    }
  }

  object Selectf {
    def unapply(tree: Tree): Option[(Tree, Symbol)] = tree match {
      case Select(qual, _) => Some((qual, tree.symbol))
      case _ => None
    }
  }

  object Selectx {
    def apply(tree: Tree, x: Symbol): Tree = {
      // TODO: this is a really fishy thing, because we should be emitting Apply(result, Nil)
      // because in most cases x is a getter symbol
      // I learned this the hard way, having accidentally forgetten to do setType to mkAttributedSelect
      // when this logic was still spread across multiple disparate code locations
      // however I'm not sure how exactly to typecheck such a tree, so I'm leaving this as is, because it works if we do do setType
      gen.mkAttributedSelect(tree, x) setType tree.tpe.memberInfo(x).finalResultType
    }
    def unapply(tree: Tree): Option[(Tree, Symbol)] = tree match {
      case Select(qual, _) if qual.valiumFields.contains(tree.symbol) => Some((qual, tree.symbol.getter))
      case Select(qual, _) if qual.valiumFields.map(_.accessed).contains(tree.symbol) => Some((qual, tree.symbol.getter))
      case Apply(Select(qual, _), Nil) if qual.valiumFields.contains(tree.symbol) => Some((qual, tree.symbol))
      case _ => None
    }
  }

  object AsInstanceOf {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Apply(TypeApplyOp(tree, Any_asInstanceOf, tpe :: Nil), Nil) => Some((tree, TypeTree(tpe)))
      case _ => None
    }
  }

  def box2unbox(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(box2unbox), List(tree)) setType tree.tpe.toUnboxedValiumRef)
  def box2unbox(sym: Symbol): Tree = box2unbox(gen.mkAttributedRef(sym))
  def unbox2box(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(unbox2box), List(tree)) setType tree.tpe.toBoxedValiumRef)
  def unbox2box(tree: Tree, x: Symbol): Tree = atPos(tree.pos)(Selectx(unbox2box(tree), x))
  def unbox2box(sym: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym))
  def unbox2box(sym: Symbol, x: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym), x)
}