package dataCollection

import database.{CarbonResult, DBQueryInterface, ProgramEntry, SiliconResult}
import util._

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.io.Directory

/** Provides methods to process submissions, create ProgramEntries, benchmark them and insert the results into the database */
object ProcessingPipeline {

  import ProcessingHelper._
  import database.BinarySerializer._
  import database.ExecContext._

  /** Combines the different processing stages, meant to be called periodically from some outside source */
  def main(args: Array[String]): Unit = {
    var tmpDirName: String = ""
    try {
      tmpDirName = programEntryStage()
    } catch {
      case NothingToDoException() => return
    }

    // TODO: write shell script that runs siliconStage and carbonStage

    filterAndInsertStage(tmpDirName)
  }

  /** Tries to get the oldest UserSubmission and create a ProgramEntry from it. Stores the ProgramEntry in the ./tmp folder for other stages to use
   *
   * @return The name of the directory in which the ProgramEntry was stored
   * @throws NothingToDoException if there are no more UserSubmissions in the database */
  def programEntryStage(): String = {
    val entryOpt = processOldestSubmission()
    entryOpt match {
      case Some(entry) => {
        val dirName = entry.hashCode().toString
        createTempDir(dirName)
        val tmpFileName = s"tmp/$dirName/programEntry.bin"
        val entryBin = serialize(entry)
        val fWriter = new BufferedOutputStream(new FileOutputStream(tmpFileName))
        try {
          fWriter.write(entryBin)
        } finally {
          fWriter.close()
        }
        dirName
      }
      case None => throw (new NothingToDoException)
    }
  }

  /** Takes the ProgramEntry stored in [[dirName]], tries to verify it through Silicon and stores the SiliconResult in ./tmp/[[dirName]]
   * Has to be run through own JVM Instance to guarantee consistency in measurements */
  def siliconStage(dirName: String): Unit = {
    val peFileName = s"tmp/$dirName/programEntry.bin"
    val fileReader = new BufferedInputStream(new FileInputStream(peFileName))
    val byteArr = try {
      fileReader.readAllBytes()
    } finally {
      fileReader.close()
    }

    val programEntry = deserialize[ProgramEntry](byteArr)

    val siliconResult = generateSiliconResults(programEntry)

    val resFileName = s"tmp/$dirName/silRes.bin"
    val resultBin = serialize(siliconResult)
    val fileWriter = new BufferedOutputStream(new FileOutputStream(resFileName))
    try {
      fileWriter.write(resultBin)
    } finally {
      fileWriter.close()
    }

  }


  /** Takes the ProgramEntry stored in [[dirName]], tries to verify it through Carbon and stores the CarbonResult in ./tmp/[[dirName]]
   * Has to be run through own JVM Instance to guarantee consistency in measurements */
  def carbonStage(dirName: String): Unit = {
    val peFileName = s"tmp/$dirName/programEntry.bin"
    val fileReader = new BufferedInputStream(new FileInputStream(peFileName))
    val byteArr = try {
      fileReader.readAllBytes()
    } finally {
      fileReader.close()
    }

    val programEntry = deserialize[ProgramEntry](byteArr)

    val carbonResult = generateCarbonResults(programEntry)

    val resFileName = s"tmp/$dirName/carbRes.bin"
    val resultBin = serialize(carbonResult)
    val fileWriter = new BufferedOutputStream(new FileOutputStream(resFileName))
    try {
      fileWriter.write(resultBin)
    } finally {
      fileWriter.close()
    }

  }

  /** Loads the generated ProgramEntry, SiliconResult and CarbonResult from [[dirName]]. Then checks if entry should be filtered out or inserted.
   * First checks if an entry that is too similar already exists in the database, if yes drops entry.
   * Then checks if entry is unique enough in its features to be added to the database, if no drops entry.
   * If filters were passed, the ProgramEntry, SiliconResult and CarbonResult are stored in the database. */
  def filterAndInsertStage(dirName: String): Unit = {
    // Loading the generated Files
    val tmpDir = s"tmp/$dirName"
    val peFileName = tmpDir + "programEntry.bin"
    val silResFileName = tmpDir + "silRes.bin"
    val carbResFileName = tmpDir + "carbRes.bin"

    val peFileReader = new BufferedInputStream(new FileInputStream(peFileName))
    val sRFileReader = new BufferedInputStream(new FileInputStream(silResFileName))
    val cRFileReader = new BufferedInputStream(new FileInputStream(carbResFileName))

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

    val programEntry = deserialize[ProgramEntry](pEByteArr)
    val siliconResult = deserialize[SiliconResult](sRByteArr)
    val carbonResult = deserialize[CarbonResult](cRByteArr)

    // Deciding whether to drop entry based on similarity
    val similarEntryExists = existsSimilarEntry(programEntry, siliconResult, carbonResult)
    if (similarEntryExists) {
      println("Entry deemed too similar, will not be stored.")
      return
    }

    //TODO: Filtering out by Features

    // Passed all filters, store in database

    val peInsert = DBQueryInterface.insertProgramEntry(programEntry)
    val sRInsert = DBQueryInterface.insertSiliconResult(siliconResult)
    val cRInsert = DBQueryInterface.insertCarbonResult(carbonResult)

    val insertQueries = Future.sequence(List(peInsert, sRInsert, cRInsert))
    Await.ready(insertQueries, Duration.Inf)
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


