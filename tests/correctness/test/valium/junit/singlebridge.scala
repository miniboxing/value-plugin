package valium.junit.singlebridge
import org.junit.Test

@value class C(val x1: Int) {
  override def toString = s"C($x1)"
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
    // println(s"$x, $y")
    x
  }
}

class SingleBridge {
  @Test def everythingWorks(): Unit = {
    (new FooForC: Foo[C]).foo(new C(0), new C(1))
    (new FooForC: Bar[C]).foo(new C(2), new C(3))
    (new FooForC: IntForC).foo(new C(4), new C(5))
    (new FooForC).foo(new C(6), new C(7))
  }
}