package dataCollection

import database.DBQueryInterface
import database.DBQueryInterface.clearDB
import org.scalatest.funsuite.AnyFunSuite
import queryFrontend.UserSubmission
import util.Config.{DEFAULT_DB_TIMEOUT, GET_CARBON_HASH_BASH_FILE, SCALA_CLASS_BASH_FILE}
import util.{getLOC, getProcessOutput}

import java.io.File
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.io.Source.fromFile
import scala.sys.process.Process

class VersionBenchmarkTest extends AnyFunSuite {

  test("Carbon version benchmark") {
    val (hash, err) = getProcessOutput(Process(GET_CARBON_HASH_BASH_FILE))
    val dbProcess = Process("./run.sh").run
    Thread.sleep(1000)
    try {
      clearDB()
      val sampleProg = readProgram(new File("src/test/resources/ProcessingTest/sample.vpr"))
      //very different program
      val sampleProg2 = readProgram(new File("src/test/resources/ProcessingTest/sample3.vpr"))

      queryFrontend.APIQueries.submitProgram("sample.vpr", sampleProg, "Silicon", Array("--timeout", "10"), "Silicon", true, 1500)
      queryFrontend.APIQueries.submitProgram("sample.vpr", sampleProg2, "Silicon", Array(), "Silicon", true, 3000)

      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline").!
      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline").!

      Process(s"$SCALA_CLASS_BASH_FILE database.tools.CarbVersionBenchmarker 4a695fff").!

      val (newhash, _) = getProcessOutput(Process(GET_CARBON_HASH_BASH_FILE))
      assert(newhash == hash)
      val progsWithoutNewHash = Await.result(DBQueryInterface.getPEIdsWithoutCarbVersionRes(newhash), DEFAULT_DB_TIMEOUT)
      val progsWithoutBMHash = Await.result(DBQueryInterface.getPEIdsWithoutCarbVersionRes("4a695fff"), DEFAULT_DB_TIMEOUT)
      assert(progsWithoutNewHash.length == 0)
      assert(progsWithoutBMHash.length == 0)

    } finally {
      dbProcess.destroy()
    }

  }

  def readProgram(file: File): String = {
    val fBuffer = fromFile(file)
    val prog =
      try fBuffer.mkString
      finally fBuffer.close()
    prog
  }

}
