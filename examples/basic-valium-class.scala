package valium.examples

@valium
class Point(val x: Double) {
  override def toString: String = 
    s"Point($x)"
}

object Test {

  def main(args: Array[String]): Unit = {
    def identity(p: Point): Point = {
      println(p.toString)
      p
    }
    
    identity(new Point(1.0))
  }
}
