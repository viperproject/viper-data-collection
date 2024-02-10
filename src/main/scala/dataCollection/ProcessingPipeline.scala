package dataCollection

import dataCollection.customFrontends.VerifierFeature
import database.{DBQueryInterface, ProgramPrintEntry}
import queryFrontend._
import util.Config._
import util._

import java.nio.channels.FileLock
import scala.concurrent.Await
import scala.sys.process._

/** Provides methods to process submissions, create ProgramEntries, benchmark them and insert the results into the database */
object ProcessingPipeline {

  import ProcessingHelper._

  private val peFileName  = "programEntry.bin"
  private val srFileName  = "siliconRes.bin"
  private val svfFileName = "siliconFeats.bin"
  private val crFileName  = "carbonRes.bin"
  private val cvfFileName = "carbonFeats.bin"
  private val ppeFileName = "programPrintEntry.bin"

  /** Combines the different processing stages, meant to be called periodically from some outside source */
  def main(args: Array[String]): Unit = {
    var tmpDirName: String   = "default"
    var globalLock: FileLock = null
    try {
      globalLock = getGlobalLock()

      if (STORE_ONLY) {
        storeOnlyStage()
      } else {
        tmpDirName = programEntryStage()

        val siliconProcess = Process(s"$SCALA_CLASS_BASH_FILE dataCollection.SiliconStageRunner $tmpDirName")
        if (siliconProcess.! == -1) throw StageIncompleteException()

        val carbonProcess = Process(s"$SCALA_CLASS_BASH_FILE dataCollection.CarbonStageRunner $tmpDirName")
        if (carbonProcess.! == -1) throw StageIncompleteException()

        filterAndInsertStage(tmpDirName)
      }
    } catch {
      case GlobalLockException() | StageIncompleteException() | NothingToDoException() => {
        removeTempDir(tmpDirName)
        if (globalLock != null) globalLock.release()
        System.exit(-1)
      }
    } finally {
      removeTempDir(tmpDirName)
      if (globalLock != null) globalLock.release()
    }
  }

  /** Tries to get the oldest UserSubmission and create a ProgramEntry from it. Stores the ProgramEntry in the ./tmp folder for other stages to use
    *
    * @return The name of the directory in which the ProgramEntry was stored
    * @throws NothingToDoException     if there are no more UserSubmissions in the database
    * @throws StageIncompleteException if there was any exception preventing the stage to complete
    */
  private def programEntryStage(): String = {
    try {
      val entryOpt = processOldestSubmission()
      entryOpt match {
        case Some(entry) => {
          val dirName = entry.hashCode().toString
          createTempDir(dirName)

          val programPrintEntry = createProgramPrintEntry(entry.program)

          storeObjectSerialized[ProgramEntry](entry, s"$TMP_DIRECTORY/$dirName/$peFileName")
          storeObjectSerialized[ProgramPrintEntry](programPrintEntry, s"$TMP_DIRECTORY/$dirName/$ppeFileName")
          dirName
        }
        case None => throw NothingToDoException()
      }
    } catch {
      case ntd: NothingToDoException => throw ntd
      case e: Exception              => e.printStackTrace(); throw StageIncompleteException()
    }

  }

  /** Takes the ProgramEntry stored in ./tmp/[[dirName]], tries to verify it through the given [[verifierFunction]] and stores the Result in ./tmp/[[dirName]]/[[outFileName]]
    *
    * Has to be run through own JVM Instance to guarantee consistency in measurements, see [[SiliconStageRunner]] and [[CarbonStageRunner]]
    *
    * @throws StageIncompleteException if there was any exception preventing the stage to complete
    */
  def verifierStage(dirName: String, verifierName: String, verifierFunction: verifierResultFunction): Unit = {
    try {
      val programEntry = loadSerializedObject[ProgramEntry](s"$TMP_DIRECTORY/$dirName/$peFileName")

      // gives runtime between min and max timeout config options
      val maxRuntime = Math.min(
        Math.max(
          ((programEntry.originalRuntime * BENCHMARK_TIMEOUT_MULTIPLIER) / 1000).toInt,
          MIN_BENCHMARK_TIMEOUT_SECONDS
        ),
        MAX_BENCHMARK_TIMEOUT_SECONDS
      )
      val (verifierResult, vFeats) = verifierFunction(programEntry, Array(), maxRuntime)

      storeObjectSerialized[VerResult](verifierResult, s"$TMP_DIRECTORY/$dirName/${verifierName}Res.bin")
      storeObjectSerialized[Array[VerifierFeature]](vFeats.toArray, s"$TMP_DIRECTORY/$dirName/${verifierName}Feats.bin")

    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }

  }

  /** Loads the generated [[ProgramEntry]], [[VerResult]]s and [[VerifierFeature]]s from ./tmp/[[dirName]].
    * Checks if an entry that is too similar already exists in the database, if yes drops entry.
    * If it wasn't dropped, the generated data is then inserted into the database.
    * @throws StageIncompleteException if there was any exception preventing the stage from completing
    */
  private def filterAndInsertStage(dirName: String): Unit = {
    try {

      val programEntry      = loadSerializedObject[ProgramEntry](s"$TMP_DIRECTORY/$dirName/$peFileName")
      val siliconResult     = loadSerializedObject[VerResult](s"$TMP_DIRECTORY/$dirName/$srFileName")
      val silVerFeats       = loadSerializedObject[Array[VerifierFeature]](s"$TMP_DIRECTORY/$dirName/$svfFileName")
      val carbonResult      = loadSerializedObject[VerResult](s"$TMP_DIRECTORY/$dirName/$crFileName")
      val carbVerFeats      = loadSerializedObject[Array[VerifierFeature]](s"$TMP_DIRECTORY/$dirName/$cvfFileName")
      val programPrintEntry = loadSerializedObject[ProgramPrintEntry](s"$TMP_DIRECTORY/$dirName/$ppeFileName")

      val procResTuple = ProcessingResultTuple(
        ProgramTuple(programEntry, programPrintEntry, siliconResult, carbonResult),
        silVerFeats,
        carbVerFeats
      )

      // Deciding whether to drop entry based on similarity
      val similarEntryExists = existsSimilarEntry(procResTuple.programTuple)
      // Passed all filters, store in database
      if (!similarEntryExists) {
        Await.ready(DBQueryInterface.insertProcessingResult(procResTuple), DEFAULT_DB_TIMEOUT)
      }
    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }
  }

  /** * Called if [[STORE_ONLY]]. Checks if new UserSubmission exists, converts it to ProgramEntry and ProgramPrintEntry,
    *  searches database for potential matching ProgramEntries and ProgramPrints,
    *  if none found, inserts ProgramEntry and ProgramPrintEntry into database.
    *
    * @throws NothingToDoException     if there are no more UserSubmissions in the database
    * @throws StageIncompleteException if there was any exception preventing the stage to complete
    */
  private def storeOnlyStage(): Unit = {
    try {
      val entryOpt = processOldestSubmission()
      entryOpt match {
        case Some(entry) => {
          val programPrintEntry = createProgramPrintEntry(entry.program)

          val existsSimilar = existsSimilarProgram(entry, programPrintEntry.programPrint)
          if (!existsSimilar) {
            Await.ready(DBQueryInterface.insertProgramAndPrint(entry, programPrintEntry), DEFAULT_DB_TIMEOUT)
          }
        }
        case None => throw NothingToDoException()
      }
    } catch {
      case ntd: NothingToDoException => throw ntd
      case e: Exception              => e.printStackTrace(); throw StageIncompleteException()
    }
  }

}

/** Case class to bundle together a [[ProgramEntry]], [[ProgramPrintEntry]] and [[VerResult]]s */
case class ProgramTuple(
  programEntry: ProgramEntry,
  programPrintEntry: ProgramPrintEntry,
  siliconResult: VerResult,
  carbonResult: VerResult
)

/** Case class to bundle together a [[programTuple]], and [[VerifierFeature]]s */
case class ProcessingResultTuple(
  programTuple: ProgramTuple,
  silVerFeatures: Array[VerifierFeature],
  carbVerFeatures: Array[VerifierFeature]
)
