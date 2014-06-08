package valium.junit.singlepoly
import org.junit.Test

@value
class Point[T](val x: T) {
  override def toString: String = {
    val p1 = this
    s"Point($x)"
  }
  override def hashCode = x.hashCode
  override def equals(other: Any) = other.isInstanceOf[Point[T]] && other.asInstanceOf[Point[T]].x == x
}

class SinglePoly {
  @Test def everythingWorks(): Unit = {
    def println(x: Any) = ()

    def identitymono(p1: Point[Double]): Point[Double] = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity2mono(p3)
      var p5 = new Point(3.0)
      p5 = p2
      p2
    }

    def identitypoly[T](p1: Point[T]): Point[T] = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity2poly(p3)
      var p5 = new Point(3.0).asInstanceOf[Point[T]]
      p5 = p2
      p2
    }

    def identity2mono(p1: Point[Double]): Point[Double] = {
      case class C1(p2: Point[Double])
      C1(p1).p2
    }

    def identity2poly[T](p1: Point[T]): Point[T] = {
      case class C11[T](p2: Point[T])
      case class C12(p2: Point[T])
      C11(p1).p2
      C12(p1).p2
    }

    def identity3mono(p1: Point[Double]): Point[Double] = {
      class C2(private[this] val p2: Point[Double]) { def y = p2 }
      new C2(p1).y
    }

    def identity3poly[T](p1: Point[T]): Point[T] = {
      class C21[T](private[this] val p2: Point[T]) { def y = p2 }
      class C22(private[this] val p2: Point[T]) { def y = p2 }
      new C21(p1).y
      new C22(p1).y
    }

    assert(identitymono(new Point(1.0)) == new Point(1.0))
    assert(identitypoly(new Point(1.0)) == new Point(1.0))
    assert(identity2mono(new Point(2.0)) == new Point(2.0))
    assert(identity2poly(new Point(2.0)) == new Point(2.0))
    assert(identity3mono(new Point(3.0)) == new Point(3.0))
    assert(identity3poly(new Point(3.0)) == new Point(3.0))
  }
}
