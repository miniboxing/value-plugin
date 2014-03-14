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
  //   11) TODO: the rule for assignments is very sloppy, but handling of bs and bm is quite elaborate. we might want to balance that
  //   12) TODO: complex expressions (blocks, ifs, try) simply fall through, but we have to remember to update their types
  //   13) TODO: do we need to transform labeldefs?
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
  // x, y => fields of the valium class with types X and Y
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
  // A02) [[ val v: VS @unboxed = cs ]] => val v$x: X = [[ unbox2box(cs).x ]]
  // A03) [[ def u[Ts](..., p: V @unboxed, ...): C = e ]] => def u[Ts](..., p$x: X, p$y: Y, ...): C = [[ e ]]
  // A04) [[ def r[Ts](...): VS @unboxed = c ]] => def r[Ts](...): X = [[ c ]]
  //
  // ======= (B) EXPRESSIONS =========
  //
  // B01) [[ unbox2box(box2unbox(e)) ]] => [[ e ]]
  // B02) [[ unbox2box(e.a).x ]] => e.a$x
  // B03) [[ unbox2box(e.a) ]] => new V(e.a$x, e.a$y)
  // B04) [[ unbox2box(bs).x ]] => [[ bs ]].asInstanceOf[X]
  // B05) [[ unbox2box(bs) ]] => new VS([[ unbox2box(bs).x ]])
  // B06) [[ box2unbox(es) ]] => [[ es ]].x.asInstanceOf[VS @unboxed]
  // B07) [[ box2unbox(em) ]] => [[ em ]]
  // B08) [[ e.a ]] => [[ e.a$x ]]
  // B09) [[ bs ]] => [[ bs ]].asInstanceOf[X]
  // B10) [[ e.u[Ts](..., a, ...) ]] => [[ e.u[Ts](..., unbox2box(a).x, unbox2box(a).y, ...) ]]
  // B11) [[ e.u[Ts](..., bs, ...) ]] => [[ { val $e = e; val $... = ...; val $bs: V @unboxed = bs; val $... = ...; $e.u[Ts]($..., $bs, $...) } ]]
  // B12) [[ e1.a1 = c2 ]] => [[ { val $c2: V @unboxed = c2; e1.a1$x = unbox2box($c2).x; e1.a1$y = unbox2box($c2).y } ]]
  // B13) [[ e1.b1 = c2 ]] => [[ { val $e1 = e1; $e1.b1 = c2 } ]]
  // B14) [[ return cs ]] => return [[ unbox2box(cs).x ]]
  class TreeConverter(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree)(implicit state: State) = {
      case ValDef(_, _, VMu(fields), am @ AM(_, _)) =>
        val exploded = fields.map(x => temp(nme.valueExplode(tree.symbol, x), unbox2box(am, x)))
        exploded.foreach(treee => tree.symbol.registerExploded(treee.symbol))
        commit(exploded)
      case ValDef(_, _, VSu(x :: Nil), cs @ CS(_, _)) =>
        val exploded = temp(nme.valueExplode(tree.symbol, x), unbox2box(cs, x))
        tree.symbol.registerExploded(exploded.symbol)
        commit(exploded)
      case DefDef(_, _, _, Vu(), _, e) =>
        commit(newDefDef(afterConvert(tree.symbol), e)() setType NoType)
      case DefDef(mods, name, tparams, vparamss, tpt @ VSu(_), c) =>
        commit(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt.toValiumField, c) setType NoType)
      case Unbox2box(Box2unbox(e)) =>
        commit(e)
      case Select(Unbox2box(A(e, a)), x) if !tree.symbol.isMethod =>
        commit(Eax(e, a, x))
      case Unbox2box(A(e, a)) =>
        val args = tree.tpe.valiumFields.map(x => Eax(e, a, x))
        commit(Apply(Select(New(TypeTree(tree.tpe)), nme.CONSTRUCTOR), args))
      case Select(Unbox2box(bs @ BS(_, _)), x) if !tree.symbol.isMethod =>
        commit(bs setType bs.tpe.toValiumField)
      case Unbox2box(bs) =>
        commit(Apply(Select(New(TypeTree(tree.tpe)), nme.CONSTRUCTOR), List(unbox2box(bs, bs.valiumField))))
      case Box2unbox(es @ ES(_, _)) =>
        commit(Select(es, es.tpe.valiumField))
      case Box2unbox(em @ EM(_, _)) =>
        commit(em)
      case A(e, a) =>
        commit(Eax(e, a, a.valiumField))
      case bs @ BS(_, _) =>
        commit(bs setType bs.tpe.toValiumField)
      case Apply(core, args) if !core.symbol.isInjected && args.exists(_.isUnboxedValiumRef) =>
        // TODO: make sure this works with varargs
        var precomputeds = List[ValDef]()
        val vals = flatMap2(args, core.tpe.params)((arg, p) => {
          if (p.isUnboxedValiumRef) {
            val precomputed = if (isB(arg) && p.valiumFields.length > 1) List(temp(nme.argPrecompute(p), arg)) else Nil
            precomputeds ++= precomputed
            val arg1 = if (precomputed.nonEmpty) atPos(arg.pos)(Ident(precomputed.head.name)) else arg
            val exploded = p.valiumFields.map(x => temp(nme.argExplode(p, x), gen.mkAttributedSelect(arg1, x)))
            precomputed ++ exploded
          } else {
            List(temp(nme.EMPTY, arg))
          }
        })
        def apply1(args1: List[Tree]) = treeCopy.Apply(tree, core.clearType(), args1).clearType()
        if (precomputeds.nonEmpty) {
          val args1 = vals.diff(precomputeds).map(vdef => Ident(vdef.name))
          commit(vals :+ apply1(args1))
        } else {
          val args1 = vals.map(_.rhs).map{ case rhs @ Select(qual, _) => rhs setType qual.tpe.memberInfo(rhs.symbol).finalResultType }
          commit(apply1(args1))
        }
      case Assign(A(e1, a1), c2 @ C(_, _)) =>
        val precomputed = temp(nme.assignPrecompute(), c2)
        commit(precomputed +: c2.valiumFields.map(x => Assign(Eax(e1, a1, x), unbox2box(precomputed.symbol, x))))
      case Assign(A(e1, b1), c2 @ C(_, _)) =>
        val precomputed = temp(nme.assignPrecompute(), e1)
        commit(List(precomputed, Assign(Select(Ident(precomputed.symbol), b1), c2)))
      case Return(cs @ CS(_, _)) =>
        commit(Select(unbox2box(cs), cs.valiumField))
    }
  }
}