package dataCollection

import database.{CarbonResult, DBQueryInterface, ProgramEntry, ProgramPrintEntry, Result, SiliconResult}
import util._
import util.Config._

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream}
import java.nio.channels.FileLock
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.io.Directory
import scala.sys.process._

/** Provides methods to process submissions, create ProgramEntries, benchmark them and insert the results into the database */
object ProcessingPipeline {

  import ProcessingHelper._
  import database.BinarySerializer._

  private val peFileName = "programEntry.bin"
  private val srFileName = "silRes.bin"
  private val crFileName = "carbRes.bin"
  private val ppeFileName = "programPrintEntry.bin"

  /** Combines the different processing stages, meant to be called periodically from some outside source */
  def main(args: Array[String]): Unit = {
    var tmpDirName: String = "default"
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
   * @throws StageIncompleteException if there was any exception preventing the stage to complete */
  private def programEntryStage(): String = {
    try {
      val entryOpt = processOldestSubmission()
      entryOpt match {
        case Some(entry) => {
          val dirName = entry.hashCode().toString
          createTempDir(dirName)

          val programPrintEntry = createProgramPrintEntry(entry.program)

          val entryFileName = s"$TMP_DIRECTORY/$dirName/$peFileName"
          val pprintFileName = s"$TMP_DIRECTORY/$dirName/$ppeFileName"
          val entryBin = serialize(entry)
          val pprintBin = serialize(programPrintEntry)
          val entryWriter = new BufferedOutputStream(new FileOutputStream(entryFileName))
          val pprintWriter = new BufferedOutputStream(new FileOutputStream(pprintFileName))
          try {
            entryWriter.write(entryBin)
          } finally {
            entryWriter.close()
          }

          try {
            pprintWriter.write(pprintBin)
          } finally {
            pprintWriter.close()
          }
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
   * @throws StageIncompleteException if there was any exception preventing the stage to complete */
  def verifierStage(dirName: String, outFileName: String, verifierFunction: (ProgramEntry, Array[String], Int) => Result): Unit = {
    val entryFileName = s"$TMP_DIRECTORY/$dirName/$peFileName"
    try {
      val fileReader = new BufferedInputStream(new FileInputStream(entryFileName))
      val byteArr = try {
        fileReader.readAllBytes()
      } finally {
        fileReader.close()
      }

      val programEntry = deserialize[ProgramEntry](byteArr)

      val maxRuntime = ((programEntry.originalRuntime * BENCHMARK_TIMEOUT_MULTIPLIER) / 1000).toInt
      val verifierResult = verifierFunction(programEntry, Array(), maxRuntime)

      val resFileName = s"$TMP_DIRECTORY/$dirName/$outFileName"
      val resultBin = serialize(verifierResult)
      val fileWriter = new BufferedOutputStream(new FileOutputStream(resFileName))
      try {
        fileWriter.write(resultBin)
      } finally {
        fileWriter.close()
      }
    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }

  }

  /** Loads the generated ProgramEntry, SiliconResult and CarbonResult from ./tmp/[[dirName]]. Then checks if entry should be filtered out or inserted.
   * First checks if an entry that is too similar already exists in the database, if yes drops entry.
   * Then checks if entry is unique enough in its features to be added to the database, if no drops entry.
   * If filters were passed, the ProgramEntry, SiliconResult and CarbonResult are stored in the database. */
  private def filterAndInsertStage(dirName: String): Unit = {
    // Loading the generated Files
    val peFile = s"$TMP_DIRECTORY/$dirName/$peFileName"
    val srFile = s"$TMP_DIRECTORY/$dirName/$srFileName"
    val crFile = s"$TMP_DIRECTORY/$dirName/$crFileName"
    val ppeFile = s"$TMP_DIRECTORY/$dirName/$ppeFileName"

    try {

      val peFileReader = new BufferedInputStream(new FileInputStream(peFile))
      val sRFileReader = new BufferedInputStream(new FileInputStream(srFile))
      val cRFileReader = new BufferedInputStream(new FileInputStream(crFile))
      val ppFileReader = new BufferedInputStream(new FileInputStream(ppeFile))

      val pEByteArr = try {
        peFileReader.readAllBytes()
      } finally {
        peFileReader.close()
      }

      val sRByteArr = try {
        sRFileReader.readAllBytes()
      } finally {
        sRFileReader.close()
      }

      val cRByteArr = try {
        cRFileReader.readAllBytes()
      } finally {
        cRFileReader.close()
      }

      val ppByteArr = try {
        ppFileReader.readAllBytes()
      } finally {
        ppFileReader.close()
      }


      val programEntry = deserialize[ProgramEntry](pEByteArr)
      val siliconResult = deserialize[SiliconResult](sRByteArr)
      val carbonResult = deserialize[CarbonResult](cRByteArr)
      val programPrintEntry = deserialize[ProgramPrintEntry](ppByteArr)

      val entryTuple = EntryTuple(programEntry, programPrintEntry, siliconResult, carbonResult)

      // Deciding whether to drop entry based on similarity
      val similarEntryExists = existsSimilarEntry(entryTuple)
      if (similarEntryExists) {
        println("Entry deemed too similar, will not be stored.")
        return
      }

      //skip feature check if fewer than 100 entries in database
      val totalEntries = Await.result(DBQueryInterface.getPECount(), DEFAULT_DB_TIMEOUT)
      if (totalEntries > 100) {
        val isInteresting = areFeaturesInteresting(entryTuple)
        if (!isInteresting) {
          println("Too many programs with similar features, will not be stored.")
          return
        }
      }

      // Passed all filters, store in database
      DBQueryInterface.insertEntry(entryTuple)
    } catch {
      case e: Exception => e.printStackTrace(); throw StageIncompleteException()
    }
  }


  private def createTempDir(dirName: String): Unit = {
    val dir = new File(s"tmp/$dirName")
    if (!dir.exists()) {
      dir.mkdir()
    }
  }

  private def removeTempDir(dirName: String): Unit = {
    val dir = new Directory(new File(s"tmp/$dirName"))
    if (dir.exists) {
      dir.deleteRecursively()
    }
  }

}



