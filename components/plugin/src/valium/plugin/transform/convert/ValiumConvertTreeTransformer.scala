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
  //    ... => anything
  //    M => any module
  //    C => any class
  //    V => any valium class
  //    VS, VM => single-field valium class and multi-field valium class
  //    m => any method
  //    u => method that has one or more of its parameters with type V @unboxed
  //    r, rs, rm => method that returns V @unboxed
  //    T => any type parameter
  //    f => any field
  //    x, y => fields of the valium class with types X and Y
  //    e, es, em => any expression, any VS boxed expression, any VM boxed expression
  //    i, is, im => any identifier, any VS @unboxed identifier, any VM @unboxed identifier
  //    c, cs, cm => anything that has type V @unboxed (a val/var with an unstable prefix, a method, an if, a match, etc)
  //    a, as, am => V.this / an ident or a select that has stable prefix, points to a val, a var or a getter and has type V @unboxed
  //    b, bs, bm => c, but not a
  //
  // ======= (A) DEFINITIONS =========
  //
  // A01) [[ val v: V @unboxed = a ]]                     => [[ val v$x: X = unbox2box(a).x; val v$y: Y = unbox2box(a).y ]]
  // A02) [[ val v: V @unboxed = bs ]]                    => [[ val v$x: X = unbox2box(bs).x ]]
  // A03) [[ val v: V @unboxed = bm ]]                    => [[ val $bm: V = unbox2box(bm); val v: V @unboxed = box2unbox($bm) ]]
  // A04) [[ val v: C = e ]]                              => val v: C = [[ e ]]
  // A05) [[ var v: V @unboxed = a ]]                     => [[ var v$x: X = unbox2box(a).x; var v$y: Y = unbox2box(a).y ]]
  // A06) [[ var v: V @unboxed = bs ]]                    => [[ var v$x: X = unbox2box(bs).x ]]
  // A07) [[ var v: V @unboxed = bm ]]                    => [[ val $bm: V = unbox2box(bm); var v: V @unboxed = box2unbox($bm) ]]
  // A08) [[ var v: C = e ]]                              => var v: C = [[ e ]]
  // A09) [[ def u[Ts](..., p: V @unboxed, ...): C = e ]] => [[ def u[Ts](..., p$x: X, p$y: Y, ...): C = e ]]
  // A10) [[ def r[Ts](...): V @unboxed = c ]]            => def r[Ts](...): X = [[ c ]]
  // A11) [[ def m[Ts](...): C = e ]]                     => def m[Ts](...): C = [[ e ]]
  // A12) [[ type T[Ts] = ... ]]                          => type T[Ts] = ...
  // A13) [[ class C[Ts] extends Cs { self => ... } ]]    => class C[Ts] extends Cs { self => [[ ... ]] }
  // A14) [[ trait C[Ts] extends Cs { self => ... } ]]    => trait C[Ts] extends Cs { self => [[ ... ]] }
  // A15) [[ object M extends Cs { self => ... } ]]       => object M extends Cs { self => [[ ... ]] }
  // A16) [[ package p { ... } ]]                         => package p { [[ ... ]] }
  //
  // ======= (B) EXPRESSIONS =========
  //
  // B01) [[ <empty tree> ]]                   => <empty tree>
  // B02) [[ <literal> ]]                      => <literal>
  // B03) [[ <new C> ]]                        => <new C>
  // B04) [[ is ]]                             => [[ unbox2box(is).x ]]
  // B05) [[ im ]]                             => im
  // B06) [[ i ]]                              => i
  // B07) [[ e.cs ]]                           => [[ e.cs$x ]]
  // B08) [[ e.cm ]]                           => [[ unbox2box(e.cm) ]]
  // B09) [[ c.x ]]                            => !!! // there can never be a naked selection from an unboxed expression
  // B10) [[ unbox2box(box2unbox(e)).x ]]      => [[ e.x ]]
  // B11) [[ unbox2box(V.this).x ]]            => V.this.x
  // B12) [[ unbox2box(e.a).x ]]               => [[ e.a$x ]]
  // B13) [[ unbox2box(bs).x ]]                => [[ bs.x ]]
  // B14) [[ unbox2box(bm).x ]]                => [[ { val $b: V @unboxed = bm; unbox2box($b).x } ]]
  // B15) [[ e.f ]]                            => [[ e ]].f
  // B16) [[ VS.this ]]                        => VS.this.x
  // B17) [[ VN.this ]]                        => VN.this
  // B18) [[ rs(...) ]]                        => [[ unbox2box(rs(...)).x ]]
  // B19) [[ rm(...) ]]                        => rm([[ ... ]])
  // B20) [[ box2unbox(es) ]]                  => [[ es ]].x
  // B21) [[ box2unbox(em) ]]                  => [[ em ]]
  // B22) [[ unbox2box(box2unbox(e)) ]]        => [[ e ]]
  // B23) [[ unbox2box(V.this) ]]              => V.this
  // B24) [[ unbox2box(e.a) ]]                 => [[ new V(unbox2box(e.a).x, unbox2box(e.a).y) ]]
  // B25) [[ unbox2box(bs) ]]                  => [[ new V1(unbox2box(bs).x) ]]
  // B26) [[ unbox2box(bm) ]]                  => [[ { val $b: VN @unboxed = bm; unbox2box($b) } ]]
  // B27) [[ e.u[Ts](..., a, ...) ]]           => [[ e.u[Ts](..., unbox2box(a).x, unbox2box(a).y, ...) ]]
  // B28) [[ e.u[Ts](..., b, ...) ]]           => [[ { val $e = e; val $... = ...; val $b: @V unboxed = b; val $... = ...; $e.u[Ts]($..., $b, $...) } ]]
  // B29) [[ e.m[Ts](...) ]]                   => [[ e ]].m[Ts]([[ ... ]])
  // B30) [[ e1.a1 = c2 ]]                     => [[ { val $c2: V @unboxed = c2; e1.a1$x = unbox2box($c2).x; e1.a1$y = unbox2box($c2).y } ]]
  // B31) [[ e1.b1 = c2 ]]                     => [[ { val $e1 = e1; $e1.b1 = c2 } ]]
  // B32) [[ e1 = e2 ]]                        => [[ e1 ]] = [[ e2 ]]
  // B33) [[ return cs ]]                      => [[ return c.x ]]
  // B34) [[ return cm ]]                      => !!! // inject only changes return values of P1-returning methods
  // B35) [[ return e ]]                       => return [[ e ]]
  // B36) [[ throw c ]]                        => ??? // can never happen, because here c will be typechecked against Exception
  // B37) [[ throw e ]]                        => throw [[ e ]]
  // B38) [[ e: C ]]                           => [[ e ]]: C
  // B39) [[ { ... } ]]                        => { [[ ... ]] }
  // B30) [[ if (cond) c1 else c2 ]]           => if ([[ cond ]]) [[ c1 ]] else [[ c2 ]]
  // B41) [[ try c catch { case p => e1 } finally e2 => try [[ c ]] catch { case p => [[ e1 ]] } finally [[ e2 ]]

  class TreeConverter(unit: CompilationUnit) extends ValuimTypingTransformer(unit) {
  }
}