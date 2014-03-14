package valium.plugin
package transform
package addext

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
  //   02) We also assume that users can't define custom getters for valium fields => these getters can be regarded as pure
  //   03) When writing out the exhaustive list of syntax forms, I used http://den.sh/quasiquotes.html#syntax-overview
  //       (of course, one has to keep in mind that some trees are desugared during parsing/typechecking and even further in the backend)
  //   04) TODO: support valium fields of valium class types
  //   05) TODO: support polymorphic value classes
  //   06) TODO: avoid boxing in cases like `val v: V @unboxed = if (cond) cl else cr`
  //   07) TODO: think whether our transformation needs to operate on patterns and types
  //   08) TODO: think whether we can avoid writing those unbox2box and box2unbox explicitly and just defer to inject/coerce
  //   09) TODO: we have to treat V.this.x references specially, because unbox2box(V.this).x doesn't typecheck. think what can be done about that
  //   10) TODO: the rule for assignments is very sloppy, but handling of bs and bm is quite elaborate. we might want to balance that
  //   11) TODO: complex expressions (blocks, ifs, try) simply fall through, but we have to remember to update their types
  //   12) TODO: do we need to transform labeldefs?
  //
  // ======= NOTATION =======
  //
  // ... => anything
  // C => any class
  // V => any valium class
  // VS, VM => single-field valium class and multi-field valium class
  // m => any method
  // u => method that has one or more of its parameters with type V @unboxed
  // r, rs, rm => method that returns V @unboxed
  // T => any type parameter
  // f => any field
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
  // B05) [[ unbox2box(bs) ]] => new V1([[ unbox2box(bs).x ]])
  // B06) [[ box2unbox(es) ]] => [[ es ]].x.asInstanceOf[VS @unboxed]
  // B07) [[ box2unbox(em) ]] => [[ em ]]
  // B08) [[ is ]] => [[ is$x ]]
  // B09) [[ e.a ]] => [[ e.a$x ]]
  // B10) [[ e.bs ]] => [[ unbox2box(e.bs).x ]]
  // B11) [[ e.u[Ts](..., a, ...) ]] => [[ e.u[Ts](..., unbox2box(a).x, unbox2box(a).y, ...) ]]
  // B12) [[ e.u[Ts](..., b, ...) ]] => [[ { val $e = e; val $... = ...; val $b: @V unboxed = b; val $... = ...; $e.u[Ts]($..., $b, $...) } ]]
  // B13) [[ e1.a1 = c2 ]] => [[ { val $c2: V @unboxed = c2; e1.a1$x = unbox2box($c2).x; e1.a1$y = unbox2box($c2).y } ]]
  // B14) [[ e1.b1 = c2 ]] => [[ { val $e1 = e1; $e1.b1 = c2 } ]]
  // B15) [[ return cs ]] => return [[ unbox2box(cs).x ]]
  class TreeConverter(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree)(implicit state: State) = {
      case ValDef(_, _, Vu(fields), a @ A()) =>
        commit(fields.map(f => temp(nme.valueExplode(tree.symbol, f), unbox2box(a, f))))
      case ValDef(_, _, Vu(f :: Nil), bs @ BS()) =>
        commit(temp(nme.valueExplode(tree.symbol, f), unbox2box(bs, f)))
      case ValDef(mods, name, tpt @ Vu(fields), bm @ BM()) =>
        val precomputed = temp(nme.valuePrecompute(tree.symbol), bm.tpe.toBoxedValiumRef, unbox2box(bm))
        val reduced = treeCopy.ValDef(tree, mods, name, tpt, box2unbox(precomputed.symbol))
        commit(precomputed :: reduced :: Nil)
      case ValDef(_, _, _, _) =>
        fallback()
    }
  }
}