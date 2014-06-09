package valium.junit.extension

@value class VS[T](val x1: T) {
  def foo[U](t: T, u: U) = t.toString + "  " + u.toString + "  " + x1.toString
}

@value class VM[T](val x1: T, val x2: Int) {
  def bar[U](t: T, u: U) = t.toString + "  " + u.toString + "  " + x1.toString + "  " + x2.toString
}

object Test {

  def main(args: Array[String]) = {
    val vs = new VS[Int](1)
    val vm = new VM[Int](2, 3)
    println(vs.hashCode())
    println(vm.hashCode())
    println(vs == (vm: Any))
    println(vs.foo[String](4, "x"))
    println(vm.bar[String](5, "y"))
  }
}
