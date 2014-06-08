package valium.junit.bottom
import org.junit.Test

@value class VS(val x1: Int)
@value class VM(val x1: Int, val x2: Int)

class Bottom {
  @Test(expected = classOf[NotImplementedError]) def nothingSingle(): Unit = {
    def test: VS = ???
    test
  }

  @Test def nullSingle(): Unit = {
    def test: VS = null
    assert(test.x1 == 0)
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