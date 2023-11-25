package dataCollection.customFrontends

import viper.silicon.{BuildInfo, Silicon, SiliconFrontend}
import viper.silver.frontend.SilFrontend
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.{BenchmarkingPhase, BenchmarkingReporter, Message, NoopReporter, Reporter}
import viper.silver.verifier.Success

import scala.collection.immutable.ArraySeq
import upickle.default.write

/** Trait for common functions shared between collection verifier frontends */
trait CollectionSilFrontend extends SilFrontend {
  private var phaseRuntimes: Seq[(String, Long)] = Seq()

  /** Runs the verifier with the given arguments */
  def main(args: Array[String]): Unit

  /** Stores the runtimes between phases in [[phaseRuntimes]] */
  override def runAllPhases(): Unit = {
    var lastTime: Long = 0
    phases.foreach(ph => {
      ph.f()
      val timeInPhase = getTime - lastTime
      lastTime = getTime
      phaseRuntimes = phaseRuntimes :+ (ph.name, timeInPhase)
    })
  }

  def hasSucceeded: Boolean = getVerificationResult match {
    case Some(res) => res match {
      case Success => true
      case _ => false
    }
    case _ => false
  }

  /** returns all features generated during verification, only valid running [[main]]. */
  def getFeatures: Seq[VerifierFeature]

  /** Only valid after calling [[main]] */
  def getPhaseRuntimes: Seq[(String, Long)] = phaseRuntimes

  /** Current commit hash of the verifier */
  def verifierHash: String

}

/** SiliconFrontend Implementation that measures runtimes in the different stages of the Verifier,
 * results can be called using [[getRuntimes]], only valid after running [[main]] */
class CollectionSiliconFrontend extends SiliconFrontend(reporter = NoopReporter, ViperStdOutLogger("Silicon", "OFF").get) with CollectionSilFrontend {
  private var benchmarkRuntimes: Seq[(String, Long)] = Seq()

  override def main(args: Array[String]): Unit = {
    try {
      execute(ArraySeq.unsafeWrapArray(args))
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    }
    finally {
      siliconInstance.reporter match {
        case bmrReporter: BenchmarkingResultReporter => benchmarkRuntimes = bmrReporter.getBenchmarkResults
        case _ => benchmarkRuntimes = Seq()
      }
      siliconInstance.stop()
    }
  }


  /** Adds a [[BenchmarkingResultReporter]] field to the Verifier s.t. benchmarking results get stored */
  override def createVerifier(fullCmd: String) = {
    siliconInstance = new Silicon(reporter, Seq("args" -> fullCmd)) {
      private val bmrReporter = BenchmarkingResultReporter()

      override def reporter: BenchmarkingResultReporter = bmrReporter
    }

    siliconInstance
  }

  def getBenchmarkResults: Seq[(String, Long)] = benchmarkRuntimes

  override def verifierHash: String = BuildInfo.gitRevision

  /** returns all features generated during verification, only valid running [[main]] */
  override def getFeatures: Seq[VerifierFeature] = {
    Seq(VerifierFeature("benchmarkResults", write(getBenchmarkResults), false))
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