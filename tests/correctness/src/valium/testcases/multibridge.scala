package valium.testcases.multibridge

@value class C(val x1: Int, val x2: Int) {
  override def toString = s"C($x1, $x2)"
}

trait Foo[X] {
  def foo(foox: X, fooy: C): X
}

trait Bar[Y] {
  def foo(barx: C, bary: Y): Y
}

trait IntForC {
  def foo(forcx: C, forcy: C): C
}

class FooForC extends Foo[C] with Bar[C] with IntForC {
  override def foo(x: C, y: C): C = {
    println(s"$x, $y")
    x
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    (new FooForC: Foo[C]).foo(new C(0, 1), new C(1, 2))
    (new FooForC: Bar[C]).foo(new C(2, 3), new C(3, 4))
    (new FooForC: IntForC).foo(new C(4, 5), new C(5, 6))
    (new FooForC).foo(new C(6, 7), new C(7, 8))
  }
}