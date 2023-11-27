package dataCollection.customFrontends

import viper.silver.reporter.{BenchmarkingPhase, Message, Reporter}

object FeatureGenerator {
  def benchmarkResToVF(res: Seq[(String, Long)]): Seq[VerifierFeature] = {
    res map {
      case (phase, time) => VerifierFeature(s"BenchmarkingPhase $phase", time.toString, false)
    }
  }

}

/** Reporter that stores the runtimes of received [[BenchmarkingPhase]]s */
case class BenchmarkingResultReporter(name: String = "benchmarking_result_reporter") extends Reporter {
  private val _initial_time = System.currentTimeMillis()
  private var _previous_phase: Long = _initial_time
  private var results: Seq[(String, Long)] = Seq()

  def report(msg: Message): Unit = {
    msg match {
      case BenchmarkingPhase(phase) =>
        val t = System.currentTimeMillis()
        results = results :+ (phase, t - _previous_phase)
        _previous_phase = t
      case _ =>
    }
  }

  def getBenchmarkResults: Seq[(String, Long)] = results
}

case class VerifierFeature(name: String, value: String, useForFiltering: Boolean) extends Serializable