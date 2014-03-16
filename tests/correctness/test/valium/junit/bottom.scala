package valium.junit.bottom
import org.junit.Test

@valium case class VS(val x1: Int)
@valium case class VM(val x1: Int, val x2: Int)

class Bottom {
  @Test(expected = classOf[NotImplementedError]) def nothingSingle(): Unit = {
    def test: VS = ???
    test
  }

  @Test def nullSingle(): Unit = {
    def test: VS = null
    assert(test == VS(0))
  }

  @Test(expected = classOf[NotImplementedError])  def nothingMulti(): Unit = {
    def test: VM = ???
    test
  }

  @Test def nullMulti(): Unit = {
    def test: VM = null
    assert(test == null)
  }
}