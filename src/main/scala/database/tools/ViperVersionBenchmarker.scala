package database.tools

import database.DBQueryInterface
import util.Config._
import util.{GlobalLockException, getGlobalLock, getGlobalLockSpinning}

import java.nio.channels.FileLock
import scala.concurrent.Await
import scala.sys.process.Process

/** Trait used to benchmark a specific version of a Viper verifier, runs benchmarks on all programs in the database and
 * adds the results to the specific VerifierResultTable */
trait ViperVersionBenchmarker {
  /** The version of the verifier to benchmark */
  var versionHash: String

  /** Takes the versionHash as an argument */
  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    versionHash = args(0)
    run()
  }

  /** Tries to change the verifier version to the specific versionHash commit, then benchmarks all programs on that
   * version, and finally restores the verifier to HEAD.
   *
   * Since this changes the local verifier version and runs benchmarks, a global lock is acquired to guarantee consistency.
   * While this lock is held, [[dataCollection.ProcessingPipeline]] will not run. */
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

  /** Should change the verifier version to [[versionHash]] and then re-compile the project */
  def swapVerifierVersion(): Int

  /** Should change the verifier version to HEAD and then re-compile the project */
  def restoreVerifierVersion(): Unit

  /** Should benchmark all programs in the database and add the results to the specific VerifierResultTable */
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
