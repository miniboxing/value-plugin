//
// This file was taken from the Rosetta Code project
// the code is under the GNU Free Documentation License 1.2
// http://www.gnu.org/licenses/fdl-1.2.html
//
// original URL: http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
//
package valium.bench.complex.scala

import scala.math.{ Pi, cos, sin, cosh, sinh, abs }

object FFT {
  def _fft(cSeq: Seq[Complex], direction: Complex, scalar: Int): Seq[Complex] = {
    if (cSeq.length == 1) {
        cSeq
    } else {
        val n = cSeq.length
        assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is a power of two.")

        val evenOddPairs = cSeq.grouped(2).toSeq
        val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
        val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)

        def leftRightPair(k: Int): (Complex, Complex) = {
            val base = evens(k) / scalar
            val offset = Complex.exp(direction * (Pi * k / n)) * odds(k) / scalar
            (base + offset, base - offset)
        }

        val pairs = (0 until n/2) map leftRightPair
        val left  = pairs map (_._1)
        val right = pairs map (_._2)
        left ++ right
    }
  }

  def  fft(cSeq: Seq[Complex]): Seq[Complex] = _fft(cSeq, Complex(0,  2), 1)
  def rfft(cSeq: Seq[Complex]): Seq[Complex] = _fft(cSeq, Complex(0, -2), 2)
}