package valium.examples

@valium
class Point(val x: Double, val y: Double) {
  override def toString: String = {
    val p1 = this
    s"Point($x, $y)"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    def identity(p1: Point): Point = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity(p3)
      var p5 = new Point(3.0, 4.0)
      p5 = p2
      p2
    }

    def identity2(p1: Point): Point = {
      case class C(p2: Point)
      C(p1).p2
    }

    identity(new Point(1.0, 2.0))
  }
}
