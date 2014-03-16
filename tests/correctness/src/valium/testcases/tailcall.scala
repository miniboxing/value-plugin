package valium.testcases.tailcalls

import annotation.tailrec

@valium class C(val i: Int)
@valium class D(val i: Int, val d: Double)

object Test {
  @tailrec def foo1(c: C): C = foo1(c)
  @tailrec def foo2(d: D): D = foo2(d)
}
