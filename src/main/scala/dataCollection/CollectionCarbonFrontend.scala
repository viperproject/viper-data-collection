package dataCollection

import viper.carbon.CarbonFrontend
import viper.silver.logger.{SilentLogger, ViperLogger, ViperStdOutLogger}
import viper.silver.reporter.{NoopReporter, StdIOReporter}
import viper.silver.verifier.{Success => CarbSuccess}

import scala.concurrent.{Await, Future, TimeoutException}

class CollectionCarbonFrontend extends CarbonFrontend(NoopReporter, ViperStdOutLogger("Carbon", "OFF").get) {
  private var phaseRuntimes: Seq[(String, Long)] = Seq()

  def main(args: Array[String]): Unit = {
    try {
      execute(Array(args(0)))
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    }
  }

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
