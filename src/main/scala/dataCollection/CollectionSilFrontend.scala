package dataCollection

import viper.silicon.{BuildInfo, Silicon, SiliconFrontend}
import viper.silver.logger.{ViperStdOutLogger}
import viper.silver.reporter.{BenchmarkingPhase, Message, NoopReporter, Reporter}
import viper.silver.verifier.{Success => SilSuccess}

import scala.collection.immutable.ArraySeq

/** SiliconFrontend Implementation that measures runtimes in the different stages of the Verifier,
 * results can be called using [[getRuntimes]], only valid after running [[runMain]] */
class CollectionSilFrontend extends SiliconFrontend(reporter = NoopReporter, ViperStdOutLogger("Silicon", "OFF").get) {
  private var phaseRuntimes: Seq[(String, Long)] = Seq()
  private var benchmarkRuntimes: Seq[(String, Long)] = Seq()
  private var benchmarkAnaResults: Seq[(String, Long)] = Seq()

  def runMain(args: Array[String]): Unit = {
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

  /** Stores the runtime between phases in [[phaseRuntimes]] */
  override def runAllPhases(): Unit = {
    var lastTime: Long = 0
    phases.foreach(ph => {
      ph.f()
      val timeInPhase = getTime - lastTime
      lastTime = getTime
      phaseRuntimes = phaseRuntimes :+ (ph.name, timeInPhase)
    })
  }

  /** Adds a [[BenchmarkingResultReporter]] field to the Verifier s.t. benchmarking results get stored */
  override def createVerifier(fullCmd: String) = {
    siliconInstance = new Silicon(reporter, Seq("args" -> fullCmd)) {
      private val bmrReporter = BenchmarkingResultReporter()

      override def reporter: BenchmarkingResultReporter = bmrReporter
    }

    siliconInstance
  }

  def getPhaseRuntimes: Seq[(String, Long)] = phaseRuntimes

  def getBenchmarkResults: Seq[(String, Long)] = benchmarkRuntimes

  def hasSucceeded: Boolean = getVerificationResult match {
    case Some(res) => res match {
      case SilSuccess => true
      case _ => false
    }
    case _ => false
  }

  def siliconHash: String = BuildInfo.gitRevision


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