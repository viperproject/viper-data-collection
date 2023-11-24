package dataCollection.customFrontends

import viper.carbon.CarbonFrontend
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.NoopReporter
import viper.silver.verifier.{TimeoutOccurred, Success => CarbSuccess}

import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.concurrent.{Await, Future}

/** Frontend to benchmark Programs using the carbon verifier */
class CollectionCarbonFrontend extends CarbonFrontend(NoopReporter, ViperStdOutLogger("Carbon", "OFF").get) {
  private var phaseRuntimes: Seq[(String, Long)] = Seq()

  /** Verifies the program. If [[timeOutSeconds]] has passed, the verifier is stopped. This is done since neither Boogie
   * nor Z3 can be passed a timeout flag that reliably works. */
  def main(args: Array[String], timeOutSeconds: Int = 0): Unit = {
    try {
      val execution = Future {
        execute(args)
      }
      if (timeOutSeconds != 0) {
        try {
          Await.ready(execution, Duration(timeOutSeconds * 1000, MILLISECONDS))
        } catch {
          case _: TimeoutException =>
            _ver.stop()
            this._errors = _errors :+ TimeoutOccurred(timeOutSeconds.toLong, "seconds")
        }
      }
      Await.ready(execution, Duration.Inf)
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    }
  }

  /** Runs the phases and adds the measured times to [[phaseRuntimes]] */
  override def runAllPhases(): Unit = {
    var lastTime: Long = 0
    phases.foreach(ph => {
      ph.f()
      val timeInPhase = getTime - lastTime
      lastTime = getTime
      phaseRuntimes = phaseRuntimes :+ (ph.name, timeInPhase)
    })
  }

  def getPhaseRuntimes: Seq[(String, Long)] = phaseRuntimes

  def hasSucceeded: Boolean = getVerificationResult match {
    case Some(res) => res match {
      case CarbSuccess => true
      case _ => false
    }
    case _ => false
  }

  //TODO: Find some way to get carbon git commit hash
  def carbonHash: String = "default"

}
