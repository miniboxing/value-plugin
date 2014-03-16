@valium
class Point[T](val x: T) {
  override def toString: String = {
    val p1 = this
    s"Point($x)"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    def identity(p1: Point[Double]): Point[Double] = {
      val p2 = p1
      def p3 = p2
      println(p2)
      println(p2.toString)
      val p4 = identity2(p3)
      var p5 = new Point(3.0)
      p5 = p2
      p2
    }

    def identity2(p1: Point[Double]): Point[Double] = {
      case class C1(p2: Point[Double])
      C1(p1).p2
    }

    def identity3[T](p1: Point[T]): Point[T] = {
      class C2[T](private[this] val p2: Point[T]) { def y = p2 }
      new C2(p1).y
    }

    println(identity(new Point(1.0)))
    println(identity(new Point(2.0)))
    println(identity(new Point(3.0)))
  }
}
