package valium.bench.complex

import org.scalameter._

object ComplexBenchmark {

  def main(args: Array[String]): Unit = {
    ComplexBench.main(Array("-silent", "-preJDK7"))
  }

  object ComplexBench extends PerformanceTest {

    def fft_size = Gen.single("data size = 2^")(4)
    def ops_size = Gen.single("data size = 2^")(14)
    def configEntries = Seq[KeyValue]() // Key.exec.jvmflags -> "-Xint")

    performance of "Complex"
      measure method "FFT" in {
        var scala_data: Seq[scala.Complex] = Seq.empty
        var valium_data: Seq[valium.Complex] = Seq.empty

        measure method "Scala Complex" in {
          using (fft_size)
          .config (configEntries: _*)
          .setUp {
            power =>
              import scala.Complex
              val rand = new util.Random(0)
              scala_data =
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              scala_data = scala_data.toVector
          }
          .tearDown {
            _ => scala_data = null
          }
          .in {
            power =>
              import scala.FFT._
              rfft(fft(scala_data))
          }
        }

        measure method "Valium Complex" in {
          using (fft_size)
          .config (configEntries: _*)
          .setUp {
            power =>
              import valium.Complex
              val rand = new util.Random(0)
              valium_data =
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              valium_data = valium_data.toVector
          }
          .tearDown {
            _ => valium_data = null
          }
          .in {
            power =>
              import valium.FFT._
              rfft(fft(valium_data))
          }
        }
      }

    performance of "Complex"
      measure method "Ops" in {
        var scala_data: Array[scala.Complex] = null
        var valium_data: Array[valium.Complex] = null

        def toArray[T: reflect.ClassTag](seq: Seq[T]): Array[T] = seq.toArray

        measure method "Scala Complex" in {
          using (ops_size)
          .config (configEntries: _*)
          .setUp {
            power =>
              import scala.Complex
              val rand = new util.Random(0)
              scala_data = toArray(
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              )
          }
          .tearDown {
            _ => scala_data = null
          }
          .in {
            power =>
              import scala.Ops._
              sum(scala_data)
          }
        }

        measure method "Valium Complex" in {
          using (ops_size)
          .config (configEntries: _*)
          .setUp {
            power =>
              import valium.Complex
              val rand = new util.Random(0)
              valium_data = toArray(
                for (i <- 1 to (1 << power))
                  yield Complex(rand.nextDouble, rand.nextDouble)
              )
          }
          .tearDown {
            _ => valium_data = null
          }
          .in {
            power =>
              import valium.Ops._
              sum(valium_data)
          }
        }
      }

    // scalameter details:
    override def executor = new execution.SeparateJvmsExecutor(
      Executor.Warmer.Default(),
      Aggregator.average,
      measurer
    )

    def measurer = new Executor.Measurer.Default
    def reporter = new reporting.LoggingReporter
    def persistor = Persistor.None
  }
}