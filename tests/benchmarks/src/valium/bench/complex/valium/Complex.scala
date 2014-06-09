//
// This file was taken from the Rosetta Code project
// the code is under the GNU Free Documentation License 1.2
// http://www.gnu.org/licenses/fdl-1.2.html
//
// original URL: http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
//
package valium.bench.complex.valium

import scala.math.{ Pi, cos, sin, cosh, sinh, abs }

@value
case class Complex(re: Double, im: Double) {
    def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
    def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
    def *(x: Double):  Complex = Complex(re * x, im * x)
    def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    def /(x: Double):  Complex = Complex(re / x, im / x)

    override def toString(): String = {
        val a = "%1.3f" format re
        val b = "%1.3f" format abs(im)
        (a,b) match {
            case (_, "0.000") => a
            case ("0.000", _) => b + "i"
            case (_, _) if im > 0 => a + " + " + b + "i"
            case (_, _) => a + " - " + b + "i"
        }
    }
}

object Complex {
  def exp(c: Complex) : Complex = {
      val r = (cosh(c.re) + sinh(c.re))
      Complex(cos(c.im), sin(c.im)) * r
  }
}
