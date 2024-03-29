package dataCollection.customFrontends

import viper.silicon.{BuildInfo, Silicon, SiliconFrontend}
import viper.silver.frontend.SilFrontend
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.NoopReporter
import viper.silver.verifier.Success

import scala.collection.immutable.ArraySeq

/** Trait for common functions shared between collection verifier frontends */
trait CollectionSilFrontend extends SilFrontend with FeatureGenerator {
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
    case Some(res) =>
      res match {
        case Success => true
        case _       => false
      }
    case _ => false
  }

  /** Only valid after calling [[main]] */
  def getPhaseRuntimes: Seq[(String, Long)] = phaseRuntimes

  /** Current commit hash of the verifier */
  def verifierHash: String

}

/** SiliconFrontend Implementation that measures runtimes in the different stages of the Verifier,
  * results can be called using [[getRuntimes]], only valid after running [[main]]
  */
class CollectionSiliconFrontend
    extends SiliconFrontend(reporter = NoopReporter, ViperStdOutLogger("Silicon", "OFF").get)
    with CollectionSilFrontend
    with SilFeatureGenerator {
  override var hasRun: Boolean                       = false
  override var syntaxProps: ProgramSyntaxProperties  = null
  private var benchmarkRuntimes: Seq[(String, Long)] = Seq()

  override def main(args: Array[String]): Unit = {
    try {
      execute(ArraySeq.unsafeWrapArray(args))
    } catch {
      case e: Exception => println(s"encountered: $e")
    } finally {
      siliconInstance.reporter match {
        case bmrReporter: BenchmarkingResultReporter => benchmarkRuntimes = bmrReporter.getBenchmarkResults
        case _                                       => benchmarkRuntimes = Seq()
      }
      siliconInstance.stop()
      instantiateSyntaxProps(args(0))
      hasRun = true
    }
  }

  /** Adds a [[BenchmarkingResultReporter]] field to the Verifier s.t. benchmarking results get stored */
  override def createVerifier(fullCmd: String): Silicon = {
    siliconInstance = new Silicon(reporter, Seq("args" -> fullCmd)) {
      private val bmrReporter = BenchmarkingResultReporter()

      override val reporter: BenchmarkingResultReporter = bmrReporter
    }

    siliconInstance
  }

  override def getBenchmarkResults: Seq[(String, Long)] = benchmarkRuntimes

  override def verifierHash: String = BuildInfo.gitRevision

}
