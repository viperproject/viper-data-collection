package database.tools

import database.DBQueryInterface
import queryFrontend.Config.TIME_DIFFERENCE_MULTIPLIER
import queryFrontend.{VerResult, VerVersionDifferenceSummary}
import util.Config._
import util.getGlobalLockSpinning

import java.nio.channels.FileLock
import scala.concurrent.Await
import scala.sys.process.Process

/** Trait used to benchmark a specific version of a Viper verifier, runs benchmarks on all programs in the database and
  * adds the results to the specific VerifierResultTable
  */
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
    * While this lock is held, [[dataCollection.ProcessingPipeline]] will not run.
    */
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

object SilVersionBenchmarker extends ViperVersionBenchmarker {
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

object CarbVersionBenchmarker extends ViperVersionBenchmarker {
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

object VersionBenchmarkHelper {
  private def generateVerVersionDiffSummary(
    vHash1: String,
    vHash2: String,
    verRes1: Seq[VerResult],
    verRes2: Seq[VerResult]
  ): VerVersionDifferenceSummary = {
    val programs1           = verRes1 map (_.programEntryId)
    val programs2           = verRes2 map (_.programEntryId)
    val programIntersection = programs1.intersect(programs2)
    val intVerRes1          = verRes1 filter (r => programIntersection.contains(r.programEntryId))
    val intVerRes2          = verRes2 filter (r => programIntersection.contains(r.programEntryId))

    val successDiff = intVerRes1.collect {
      case vr1 if intVerRes2.exists(vr2 => vr1.programEntryId == vr2.programEntryId && vr1.success != vr2.success) =>
        vr1.programEntryId
    }

    val runtimeDiff = intVerRes1.collect {
      case vr1
          if intVerRes2.exists(vr2 =>
            vr1.programEntryId == vr2.programEntryId && (vr1.runtime <= vr2.runtime / TIME_DIFFERENCE_MULTIPLIER
            || vr1.runtime >= vr2.runtime * TIME_DIFFERENCE_MULTIPLIER)
          ) =>
        vr1.programEntryId
    }

    val errorDiff = intVerRes1.collect {
      case vr1
          if intVerRes2.exists(vr2 => vr1.programEntryId == vr2.programEntryId && vr1.errorIdSet != vr2.errorIdSet) =>
        vr1.programEntryId
    }

    val avgRuntime1 = if (intVerRes1.isEmpty) 0 else intVerRes1.map(_.runtime).sum / intVerRes1.length
    val avgRuntime2 = if (intVerRes2.isEmpty) 0 else intVerRes2.map(_.runtime).sum / intVerRes2.length

    val runtimeVar1 =
      if (intVerRes1.isEmpty) 0 else intVerRes1.map(vr => Math.pow(vr.runtime - avgRuntime1, 2)).sum / intVerRes1.length
    val runtimeVar2 =
      if (intVerRes2.isEmpty) 0 else intVerRes2.map(vr => Math.pow(vr.runtime - avgRuntime2, 2)).sum / intVerRes2.length

    VerVersionDifferenceSummary(
      vHash1,
      vHash2,
      programIntersection,
      successDiff,
      runtimeDiff,
      errorDiff,
      avgRuntime1,
      avgRuntime2,
      runtimeVar1.toLong,
      runtimeVar2.toLong
    )
  }

  def generateSilVersionDiffSummary(vHash1: String, vHash2: String): VerVersionDifferenceSummary = {
    val silRes1 = Await.result(DBQueryInterface.getSilResForVersion(vHash1), DEFAULT_DB_TIMEOUT)
    val silRes2 = Await.result(DBQueryInterface.getSilResForVersion(vHash2), DEFAULT_DB_TIMEOUT)
    generateVerVersionDiffSummary(vHash1, vHash2, silRes1, silRes2)
  }

  def generateCarbVersionDiffSummary(vHash1: String, vHash2: String): VerVersionDifferenceSummary = {
    val carbRes1 = Await.result(DBQueryInterface.getCarbResForVersion(vHash1), DEFAULT_DB_TIMEOUT)
    val carbRes2 = Await.result(DBQueryInterface.getCarbResForVersion(vHash2), DEFAULT_DB_TIMEOUT)
    generateVerVersionDiffSummary(vHash1, vHash2, carbRes1, carbRes2)
  }
}
