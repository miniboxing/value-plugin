package valium.plugin
package transform
package convert

trait ValiumConvertTreeTransformer {
  this: ValiumConvertPhase =>

  import global._
  import definitions._
  import treeInfo._
  import helper._
  import Flag._

  // ======= NOTES =======
  //
  //   01) Value classes are assumed to be immutable, because mutability and unboxing can't coexist
  //   02) We also assume absense of side-effects in value class constructors
  //   03) We also assume that users can't define custom getters for valium fields => these getters can be regarded as pure
  //   04) When writing out the exhaustive list of syntax forms, I used http://den.sh/quasiquotes.html#syntax-overview
  //       (of course, one has to keep in mind that some trees are desugared during parsing/typechecking and even further in the backend)
  //   05) TODO: support valium fields of valium class types
  //   06) TODO: support polymorphic value classes
  //   07) TODO: avoid boxing in cases like `val v: V @unboxed = if (cond) cl else cr`
  //   08) TODO: think whether our transformation needs to operate on patterns and types
  //   09) TODO: think whether we can avoid writing those unbox2box and box2unbox explicitly and just defer to inject/coerce
  //   10) TODO: we have to treat V.this.x references specially, because unbox2box(V.this).x doesn't typecheck. think what can be done about that
  //   11) TODO: complex expressions (blocks, ifs, try) simply fall through, but we have to remember to update their types
  //   12) TODO: do we need to transform labeldefs?
  //   13) TODO: make sure that varargs in method calls and constructor invocations work fine
  //
  // ======= NOTATION =======
  //
  // ... => anything
  // C => any class
  // V => any valium class
  // VS, VM => single-field valium class and multi-field valium class
  // u => method that has one or more of its parameters with type V @unboxed
  // r => method that returns V @unboxed
  // T => any type parameter
  // x, y => underlying fields or getters of the valium class with types X and Y
  // f => any selector
  // e, es, em => any expression, any VS boxed expression, any VM boxed expression
  // i, is, im => any identifier, any VS @unboxed identifier, any VM @unboxed identifier
  // c, cs, cm => anything that has type V @unboxed (a val/var with an unstable prefix, a method, an if, a match, etc)
  // a, as, am => an ident or a select that has stable prefix, points to a val, a var or a getter and has type V @unboxed
  // b, bs, bm => c, but not a
  //
  // INVARIANT: there is no bm (TODO: Implement this!)
  // INVARIANT: V.this: V
  //
  // ======= (A) DEFINITIONS =========
  //
  // A01) [[ val v: VM @unboxed = am ]] => val v$x: X = [[ unbox2box(am).x ]]; val v$y: Y = [[ unbox2box(am).y ]]
  // A02) [[ val v: VM @unboxed = box2unbox(new V(e1, e2)) ]] => val v$x: X = [[ e1 ]]; val v$y: Y = [[ e2 ]]
  // A03) [[ val v: VM @unboxed = box2unbox(em) ]] => val $em: VM = [[ em ]]; val v$x: X = $em.x; val v$y: Y = $em.y
  // A04) [[ val v: VS @unboxed = cs ]] => val v$x: X = [[ unbox2box(cs).x ]]
  // A05) [[ val v: V @unboxed = _ ]] => val v$x: X = _; val v$y: Y = _
  // A06) [[ def u[Ts](..., p: V @unboxed, ...): C = e ]] => def u[Ts](..., p$x: X, p$y: Y, ...): C = [[ e ]]
  // A07) [[ def r[Ts](...): VS @unboxed = c ]] => def r[Ts](...): X = [[ c ]]
  //
  // ======= (B) EXPRESSIONS =========
  //
  // B01) [[ unbox2box(box2unbox(e)).f ]] => [[ e ]].f
  // B02) [[ unbox2box(box2unbox(e)) ]] => [[ e ]]
  // B03) [[ unbox2box(e.a).x ]] => e.a$x
  // B04) [[ unbox2box(e.a) ]] => new V(e.a$x, e.a$y)
  // B05) [[ unbox2box(cs).x ]] => [[ cs ]].asInstanceOf[X]
  // B06) [[ unbox2box(cs) ]] => new VS([[ unbox2box(cs).x ]])
  // B07) [[ box2unbox(es) ]] => [[ es ]].x.asInstanceOf[VS @unboxed]
  // B08) [[ box2unbox(em) ]] => [[ em ]]
  // B09) [[ e.a ]] => [[ e.a$x ]]
  // B10) [[ bs ]] => [[ bs ]].asInstanceOf[X]
  // B11) [[ e.u[Ts](..., a, ...) ]] => [[ e.u[Ts](..., unbox2box(a).x, unbox2box(a).y, ...) ]]
  // B12) [[ e.u[Ts](..., b, ...) ]] => [[ { val $e = e; val $... = ...; val $b: V @unboxed = b; val $... = ...; $e.u[Ts]($..., $b, $...) } ]]
  // B13) [[ e1.a1 = a2 ]] => [[ { e1.a1$x = unbox2box(a2).x; e1.a1$y = unbox2box(a2).y } ]]
  // B14) [[ e1.a1 = b2 ]] => [[ { val $b2: V @unboxed = b2; e1.a1 = $b2 } ]]
  // B15) [[ e1.b1 = c2 ]] => [[ { val $e1 = e1; $e1.b1 = c2 } ]]
  // B16) [[ return cs ]] => return [[ unbox2box(cs).x ]]
  // B17) [[ new V(e1, e2).x ]] => [[ e1 ]]
  class TreeConverter(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree)(implicit state: State) = {
      case ValDef(_, _, VMu(fields), am @ AM(_, _)) =>
        val exploded = fields.map(x => temp(nme.valueExplode(tree.symbol, x), unbox2box(am, x)))
        exploded.foreach(treee => tree.symbol.registerExploded(treee.symbol))
        commit("A01", exploded)
      case ValDef(_, _, VMu(fields), Box2unbox(Apply(Select(New(V(_)), nme.CONSTRUCTOR), args))) =>
        val exploded = fields.zip(args).map{ case (x, e) => temp(nme.valueExplode(tree.symbol, x), e) }
        exploded.foreach(treee => tree.symbol.registerExploded(treee.symbol))
        commit("A02", exploded)
      case ValDef(_, _, VMu(fields), Box2unbox(em @ EM(_, _))) =>
        val precomputed = temp(nme.valuePrecompute(tree.symbol), em)
        val exploded = fields.map(x => temp(nme.valueExplode(tree.symbol, x), Selectx(gen.mkAttributedRef(precomputed.symbol), x)))
        exploded.foreach(treee => tree.symbol.registerExploded(treee.symbol))
        commit("A03", precomputed +: exploded)
      case ValDef(_, _, VMu(fields), bm @ BM(_, _)) =>
        error(s"unauthorized bm detected: $tree")
      case ValDef(_, _, VSu(x :: Nil), cs @ CS(_, _)) =>
        val exploded = temp(nme.valueExplode(tree.symbol, x), unbox2box(cs, x))
        tree.symbol.registerExploded(exploded.symbol)
        commit("A04", exploded)
      case ValDef(_, _, tpt @ Vu(fields), EmptyTree) =>
        val exploded = fields.map(x => temp(nme.valueExplode(tree.symbol, x), tpt.tpe.memberInfo(x).finalResultType, EmptyTree))
        exploded.foreach(treee => tree.symbol.registerExploded(treee.symbol))
        tree.symbol.owner.info.decls.unlink(tree.symbol)
        commit("A05", exploded)
      case DefDef(_, _, _, Vuss(), _, e) =>
        val tree1 = newDefDef(afterConvert(tree.symbol), e)() setType NoType
        tree1.vparamss.flatten.foreach(_ setType NoType)
        commit("A06", tree1)
      case DefDef(mods, name, tparams, vparamss, tpt @ VSu(_), c) =>
        commit("A07", treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt.toValiumField, c) setType NoType)
      case DefDef(mods, name, tparams, vparamss, tpt @ VMu(_), c) =>
        error(s"unauthorized bm detected: $tree")
      case Selectf(Unbox2box(Box2unbox(e)), f) =>
        commit("B01", Select(e, f))
      case Unbox2box(Box2unbox(e)) =>
        commit("B02", e)
      case Selectx(Unbox2box(A(e, a)), x) =>
        commit("B03", Eax(e, a, x))
      case Unbox2box(A(e, a)) =>
        val args = tree.tpe.valiumFields.map(x => Eax(e, a, x))
        commit("B04", Apply(Select(New(TypeTree(tree.tpe)), nme.CONSTRUCTOR), args))
      case Selectx(Unbox2box(cs @ CS(_, _)), x) =>
        commit("B05", cs setType cs.tpe.toValiumField)
      case Unbox2box(cs @ CS(_, _)) =>
        commit("B06", Apply(Select(New(TypeTree(tree.tpe)), nme.CONSTRUCTOR), List(unbox2box(cs, cs.valiumField))))
      case Unbox2box(bm @ BM(_, _)) =>
        error(s"unauthorized bm detected: $tree")
      case Box2unbox(es @ ES(_, _)) =>
        commit("B07", Select(es, es.tpe.valiumField))
      case Box2unbox(em @ EM(_, _)) =>
        commit("B08", em)
      case A(e, a) =>
        commit("B09", Eax(e, a, a.valiumField))
      case bs @ BS(_, _) =>
        commit("B10", bs setType bs.tpe.toValiumField)
      case bm @ BM(_, _) =>
        error(s"unauthorized bm detected: $tree")
      case U(core, args) =>
        // TODO: implement prefix precomputation
        var precomputeds = List[ValDef]()
        val vals = flatMap2(args, core.tpe.params)((arg, p) => {
          if (p.isUnboxedValiumRef) {
            val precomputed = if (isB(arg) && p.valiumFields.length > 1) List(temp(nme.argPrecompute(p), arg)) else Nil
            precomputeds ++= precomputed
            val arg1 = if (precomputed.nonEmpty) atPos(arg.pos)(gen.mkAttributedIdent(precomputed.head.symbol)) else arg
            val exploded = p.valiumFields.map(x => temp(nme.argExplode(p, x), unbox2box(arg1, x)))
            precomputed ++ exploded
          } else {
            List(temp(nme.EMPTY, arg))
          }
        })
        def apply1(args1: List[Tree]) = treeCopy.Apply(tree, core.clearType(), args1).clearType()
        if (precomputeds.nonEmpty) {
          val args1 = vals.diff(precomputeds).map(vdef => Ident(vdef.symbol))
          commit("B12", vals :+ apply1(args1))
        } else {
          val args1 = vals.map(_.rhs).map{ case rhs @ Select(qual, _) => rhs setType qual.tpe.memberInfo(rhs.symbol).finalResultType }
          commit("B11", apply1(args1))
        }
      case Assign(A(e1, a1), a2 @ A(_, _)) =>
        commit("B13", a2.valiumFields.map(x => Assign(Eax(e1, a1, x), unbox2box(a2, x))))
      case Assign(lhs @ A(e1, a1), b2 @ B(_, _)) =>
        val precomputed = temp(nme.assignPrecompute(), b2)
        commit("B14", List(precomputed, lhs, Ident(precomputed.symbol)))
      case Assign(A(e1, b1), c2 @ C(_, _)) =>
        val precomputed = temp(nme.assignPrecompute(), e1)
        commit("B15", List(precomputed, Assign(Select(Ident(precomputed.symbol), b1), c2)))
      case Return(cs @ CS(_, _)) =>
        commit("B16", Select(unbox2box(cs), cs.valiumField))
      case Selectx(Apply(Select(New(V(fields)), nme.CONSTRUCTOR), args), x) =>
        commit("B17", args(fields.indexOf(x)))
    }
  }
}