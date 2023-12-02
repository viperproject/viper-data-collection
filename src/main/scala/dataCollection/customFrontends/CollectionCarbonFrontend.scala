package dataCollection.customFrontends

import util.Config.GET_CARBON_HASH_BASH_FILE
import util.getProcessOutput
import viper.carbon.CarbonFrontend
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.NoopReporter
import viper.silver.verifier.{TimeoutOccurred, Success => CarbSuccess}

import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.concurrent.{Await, Future}
import scala.sys.process.Process

/** Frontend to benchmark programs using the Carbon verifier, takes seconds to timeout as an argument, 0 => no timeout */
class CollectionCarbonFrontend(timeOut: Int = 0)
    extends CarbonFrontend(NoopReporter, ViperStdOutLogger("Carbon", "OFF").get)
    with CollectionSilFrontend
    with CarbFeatureGenerator {
  override var hasRun: Boolean                      = false
  override var syntaxProps: ProgramSyntaxProperties = null

  /** Verifies the program. If [[timeOut]] seconds have passed, the verifier is stopped. This is done since neither Boogie
    * nor Z3 can be passed a timeout flag that reliably works.
    */
  override def main(args: Array[String]): Unit = {
    try {
      val execution = Future {
        execute(args)
      }
      if (timeOut != 0) {
        try {
          Await.ready(execution, Duration(timeOut * 1000, MILLISECONDS))
        } catch {
          case _: TimeoutException =>
            _ver.stop()
            this._errors = _errors :+ TimeoutOccurred(timeOut.toLong, "seconds")
        }
      }
      Await.ready(execution, Duration.Inf)
      instantiateSyntaxProps(args(0))
      hasRun = true
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    }
  }
  
  def verifierHash: String = {
    val (hash, err) = getProcessOutput(Process(GET_CARBON_HASH_BASH_FILE))
    if (err != "") "default" else hash
  }
}
