package dataCollection

import viper.carbon.CarbonFrontend
import viper.silver.logger.{SilentLogger, ViperLogger}
import viper.silver.reporter.{NoopReporter, StdIOReporter}

import scala.concurrent.{Await, Future, TimeoutException}

class CollectionCarbonFrontend extends CarbonFrontend(NoopReporter, logger = SilentLogger().get) {
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

}
