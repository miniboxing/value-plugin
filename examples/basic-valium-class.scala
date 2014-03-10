package valium.examples

@valium
class Point(val x: Double) {
  override def toString: String =
    s"Point($x)"
}

object Test {
  def main(args: Array[String]): Unit = {
    def identity(p1: Point): Point = {
      val p2 = p1
      println(p2.toString)
      p2
    }

    identity(new Point(1.0))
  }
}
