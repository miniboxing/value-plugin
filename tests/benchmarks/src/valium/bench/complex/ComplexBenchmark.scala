package valium.bench.complex

import org.scalameter._

object ComplexBenchmark {

  def main(args: Array[String]): Unit = {
    ComplexBench.main(Array("-silent", "-preJDK7"))
  }

  object ComplexBench extends PerformanceTest.Quickbenchmark {

    override def executor = new execution.LocalExecutor(
      Executor.Warmer.Default(),
      Aggregator.average,
      measurer
    )
    val size = Gen.single("data size = 2^")(13)


    performance of "FFT"
      measure method "Complex" in {
        var scala_data: Seq[scala.Complex] = Seq.empty
        var valium_data: Seq[valium.Complex] = Seq.empty

        measure method "Scala Complex" in {
          using (size) setUp {
            power =>
              import scala.Complex
              val rand = new util.Random(0)
              scala_data =
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              scala_data = scala_data.toVector
          } tearDown {
            _ => scala_data = null
          } in {
            power =>
              import scala.FFT._
              rfft(fft(scala_data))
          }
        }

        measure method "Valium Complex" in {
          using (size) setUp {
            power =>
              import valium.Complex
              val rand = new util.Random(0)
              valium_data =
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              valium_data = valium_data.toVector
          } tearDown {
            _ => valium_data = null
          } in {
            power =>
              import valium.FFT._
              rfft(fft(valium_data))
          }
        }
      }
  }
}