@value class VS(val x1: Int)
@value class VM(val x1: Int, val x2: Int)

object Test {
  def foos: VS = ???
  def bars: VS = null
  def foom: VM = ???
  def barm: VM = null
}