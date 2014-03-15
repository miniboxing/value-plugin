package valium.examples

@valium
class Point(val x: Double) {
  override def toString: String = {
    val p1 = this
    s"Point($x)"
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

    println(identity(new Point(1.0)))
    println(identity(new Point(2.0)))
    println(identity(new Point(3.0)))
  }
}
