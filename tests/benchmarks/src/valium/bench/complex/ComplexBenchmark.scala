package valium.bench.complex

import org.scalameter._

object ComplexBenchmark {

  def main(args: Array[String]): Unit = {
    ComplexBench.main(Array("-silent", "-preJDK7"))
  }

  object ComplexBench extends PerformanceTest.Quickbenchmark {

    val size = Gen.single("data size = 2^")(13)

    var scala_data: Seq[scala.Complex] = Seq.empty
    performance of "Scala Complex"
      using (size) setUp {
        power =>
          import scala.Complex
          val rand = new util.Random(0)
          scala_data =
            for (i <- 1 to (1 << power))
              yield new Complex(rand.nextDouble, rand.nextDouble)
      } in {
        power =>
          import scala.FFT._
          rfft(fft(scala_data))
      }

    var valium_data: Seq[valium.Complex] = Seq.empty
    performance of "Valium Complex"
      using (size) setUp {
        power =>
          import valium.Complex
          val rand = new util.Random(0)
          valium_data =
            for (i <- 1 to (1 << power))
              yield new Complex(rand.nextDouble, rand.nextDouble)
      } in {
        power =>
          import valium.FFT._
          rfft(fft(valium_data))
      }
  }
}