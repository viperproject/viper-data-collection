package dataCollection

import dataCollection.customFrontends.VerifierFeature
import database.{ DBQueryInterface, ProgramEntry, ProgramPrintEntry, VerResult }
import util._
import util.Config._

import java.nio.channels.FileLock
import scala.concurrent.{ Await }
import scala.sys.process._

/** Provides methods to process submissions, create ProgramEntries, benchmark them and insert the results into the database */
object ProcessingPipeline {

  import ProcessingHelper._
  import database.BinarySerializer._

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
      if (globalLock == null) return

      tmpDirName = programEntryStage()

      val siliconProcess = Process(s"$SCALA_CLASS_BASH_FILE dataCollection.SiliconStageRunner $tmpDirName")
      if (siliconProcess.! == -1) return

      val carbonProcess = Process(s"$SCALA_CLASS_BASH_FILE dataCollection.CarbonStageRunner $tmpDirName")
      if (carbonProcess.! == -1) return

      filterAndInsertStage(tmpDirName)
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
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
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

      val maxRuntime               = ((programEntry.originalRuntime * BENCHMARK_TIMEOUT_MULTIPLIER) / 1000).toInt
      val (verifierResult, vFeats) = verifierFunction(programEntry, Array(), maxRuntime)

      storeObjectSerialized[VerResult](verifierResult, s"$TMP_DIRECTORY/$dirName/${verifierName}Res.bin")
      storeObjectSerialized[Array[VerifierFeature]](vFeats.toArray, s"$TMP_DIRECTORY/$dirName/${verifierName}Feats.bin")

    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }

  }

  /** Loads the generated ProgramEntry, SiliconResult and CarbonResult from ./tmp/[[dirName]]. Then checks if entry should be filtered out or inserted.
    * First checks if an entry that is too similar already exists in the database, if yes drops entry.
    * Then checks if entry is unique enough in its features to be added to the database, if no drops entry.
    * If filters were passed, the ProgramEntry, SiliconResult and CarbonResult are stored in the database.
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
      if (similarEntryExists) {
        println("Entry deemed too similar, will not be stored.")
        return
      }

      //TODO: Use features
      //skip feature check if fewer than 100 entries in database
      val totalEntries = Await.result(DBQueryInterface.getPECount(), DEFAULT_DB_TIMEOUT)
      if (totalEntries > 100) {
        val shouldDropByMeta = shouldDropByMetadata(procResTuple.programTuple)
        if (shouldDropByMeta) {
          println("Too many programs with similar metadata, will not be stored.")
          return
        }
        val shouldDropByFeat = shouldDropByFeatures(procResTuple.silVerFeatures ++ procResTuple.carbVerFeatures)
        if (shouldDropByFeat) {
          println("Too many programs with similar features, will not be stored.")
          return
        }
      }

      // Passed all filters, store in database
      Await.ready(DBQueryInterface.insertProcessingResult(procResTuple), DEFAULT_DB_TIMEOUT)
    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }
  }

}

case class ProgramTuple(
    programEntry: ProgramEntry,
    programPrintEntry: ProgramPrintEntry,
    siliconResult: VerResult,
    carbonResult: VerResult
)

case class ProcessingResultTuple(
    programTuple: ProgramTuple,
    silVerFeatures: Array[VerifierFeature],
    carbVerFeatures: Array[VerifierFeature]
)
