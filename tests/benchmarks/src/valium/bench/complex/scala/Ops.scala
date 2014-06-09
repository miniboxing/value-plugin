package valium.bench.complex.scala

object Ops {

  def sum(seq: Array[Complex]): Complex = {
    var acc = Complex(0.0, 0.0)
    var i = 0
    while (i < seq.size) {
      acc += seq(i)
      i += 1
    }
    acc
  }

  def prod(seq: Array[Complex]): Complex = {
    var acc = Complex(0.0, 0.0)
    var i = 0
    while (i < seq.size) {
      acc += seq(i)
      i += 1
    }
    acc
  }
}