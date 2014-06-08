package valium.junit.single
import org.junit.Test

@value
class Point(val x: Double) {
  override def toString: String = {
    val p1 = this
    s"Point($x)"
  }
  override def hashCode = x.hashCode
  override def equals(other: Any) = other.isInstanceOf[Point] && other.asInstanceOf[Point].x == x
}

class Single {
  @Test def everythingWorks(): Unit = {
    def println(x: Any) = ()

    def identity(p1: Point): Point = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity2(p3)
      var p5 = new Point(3.0)
      p5 = p2
      p2
    }

    def identity2(p1: Point): Point = {
      case class C1(p2: Point)
      C1(p1).p2
    }

    def identity3(p1: Point): Point = {
      class C2(private[this] val p2: Point) { def y = p2 }
      new C2(p1).y
    }

    assert(identity(new Point(1.0)) == new Point(1.0))
    assert(identity2(new Point(2.0)) == new Point(2.0))
    assert(identity3(new Point(3.0)) == new Point(3.0))
  }
}
