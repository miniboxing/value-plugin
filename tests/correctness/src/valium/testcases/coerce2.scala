package valium.testsuite.coerce2

@value
class Point(val x: Double, val y: Double)

class Points(val p1: Point, val p2: Point)

object Test {
  def foo(p: Point) = p

  def main(args: Array[String]): Unit = {

    // should not use boxing:
    val p1 = new Point(1.0, 2.0)
    val p2 = new Point(2.0, 3.0)
    val p3 = p1
    val p4 = p2
    val ps = new Points(p1, p2)

    // should use boxing:
    val boxed = {
      val p5 = if (???) p1 else p2
      val p6 = { println("hi"); p1 }
      val p7 = foo(p1)
      val p8 = ps.p1
      val p9 = ps.p2
    }
  }
}
