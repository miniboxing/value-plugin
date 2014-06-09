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

object Ops {

  def sum(seq: Seq[Complex]): Complex = {
    var acc = Complex(0.0, 0.0)
    var i = 0
    while (i < seq.size) {
      acc = acc + seq(i)
      i += 1
    }
    acc
  }

  def prod(seq: Seq[Complex]): Complex = {
    var acc = Complex(0.0, 0.0)
    var i = 0
    while (i < seq.size) {
      acc = acc * seq(i)
      i += 1
    }
    acc
  }
}
