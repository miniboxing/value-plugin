@valium class C(val x1: Int)

trait Foo[X] {
  def foo(x: X, y: C): X
}

trait Bar[Y] {
  def foo(x: C, y: Y): Y
}

trait IntForC {
  def foo(c: C, y: C): C
}

class FooForC extends Foo[C] with Bar[C] with IntForC {
  override def foo(x: C, y: C): C = {
    println("it works")
    x
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    (new FooForC: Foo[C]).foo(null, null)
    (new FooForC: Bar[C]).foo(null, null)
    (new FooForC: IntForC).foo(null, null)
    (new FooForC).foo(null, null)
  }
}