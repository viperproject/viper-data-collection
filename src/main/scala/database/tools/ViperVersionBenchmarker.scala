package database.tools

import database.DBQueryInterface
import util.Config._
import util.{GlobalLockException, getGlobalLock, getGlobalLockSpinning}

import java.nio.channels.FileLock
import scala.concurrent.Await
import scala.sys.process.Process

trait ViperVersionBenchmarker {
  var versionHash: String

  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    versionHash = args(0)
    run()
  }

  def run(): Unit = {
    var globalLock: FileLock = null
    try {
      globalLock = getGlobalLockSpinning()
      if (swapVerifierVersion() != -1) {
        benchmark()
      }
    } finally {
      restoreVerifierVersion()
      if (globalLock != null) globalLock.release()
    }
  }

  def swapVerifierVersion(): Int

  def restoreVerifierVersion(): Unit

  def benchmark(): Unit
}

class SilVersionBenchmarker extends ViperVersionBenchmarker {
  override var versionHash: String = ""


  override def swapVerifierVersion(): Int = {
    Process(s"$SWITCH_SIL_VERSION_BASH_FILE $versionHash").!
  }

  override def restoreVerifierVersion(): Unit = {
    Process(RESTORE_SIL_VERSION_BASH_FILE).!
  }

  override def benchmark(): Unit = {
    val peIdsWithoutRes = Await.result(DBQueryInterface.getPEIdsWithoutSilVersionRes(versionHash), DEFAULT_DB_TIMEOUT)
    for (id <- peIdsWithoutRes) {
      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.SiliconBenchmarkRunner $id").!
    }
  }
}

class CarbVersionBenchmarker extends ViperVersionBenchmarker {
  override var versionHash: String = ""


  override def swapVerifierVersion(): Int = {
    Process(s"$SWITCH_CARB_VERSION_BASH_FILE $versionHash").!
  }

  override def restoreVerifierVersion(): Unit = {
    Process(RESTORE_CARB_VERSION_BASH_FILE).!
  }

  override def benchmark(): Unit = {
    val peIdsWithoutRes = Await.result(DBQueryInterface.getPEIdsWithoutCarbVersionRes(versionHash), DEFAULT_DB_TIMEOUT)
    for (id <- peIdsWithoutRes) {
      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.CarbonBenchmarkRunner $id").!
    }
  }
}
