package valium.junit
import org.junit.Test

@valium
class C(val x: Int, val y: Int)

class Basic {
  @Test def everythingWorks() = ()
}