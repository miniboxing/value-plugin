package valium.junit.multi
import org.junit.Test

@value
class Point(val x: Double, val y: Double) {
  override def toString: String = {
    val p1 = this
    s"Point($x, $y)"
  }
  override def hashCode = x.hashCode + y.hashCode
  override def equals(other: Any) = other.isInstanceOf[Point] && other.asInstanceOf[Point].x == x && other.asInstanceOf[Point].y == y
}

class Multi {
  @Test def everythingWorks(): Unit = {
    def println(x: Any) = ()

    def identity(p1: Point): Point = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity2(p3)
      var p5 = new Point(3.0, 3.1)
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

    assert(identity(new Point(1.0, 1.1)) == new Point(1.0, 1.1))
    assert(identity2(new Point(2.0, 2.1)) == new Point(2.0, 2.1))
    assert(identity3(new Point(3.0, 3.1)) == new Point(3.0, 3.1))
  }
}
