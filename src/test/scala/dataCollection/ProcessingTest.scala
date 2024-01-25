package dataCollection

import database.tools.PatternMatcher
import queryFrontend._
import org.scalatest.funsuite.AnyFunSuite
import ujson.{Arr, Obj}
import util._
import util.Config._

import java.io.File
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.io.Source.fromFile
import scala.sys.process.Process

class ProcessingTest extends AnyFunSuite {

  val host = s"http://localhost:$WEBSERVER_LOCAL_PORT"

  import database.DBQueryInterface._

  /*test("Backup and restore") {
    val dbProcess = Process("docker-compose up").run
    Thread.sleep(1000)
    try {
      Await.ready(clearDB(), DEFAULT_DB_TIMEOUT)
      Await.ready(insertFeature(Feature(0, "test", false)), DEFAULT_DB_TIMEOUT)
      Process("bash_scripts/db_backup.sh").!
      Await.ready(clearDB(), DEFAULT_DB_TIMEOUT)
      Process("bash_scripts/db_restore.sh dump.sql").!
      assert(Await.result(getFeatCount(), DEFAULT_DB_TIMEOUT) == 1)
    } finally {
      dbProcess.destroy()
    }

  }*/

  /** IMPORTANT: This test will clear the database, do not run once actually in use */
  /*test("Global lock test") {
    val dbProcess = Process("./run.sh").run
    Thread.sleep(1000)

    try {
      Await.ready(clearDB(), DEFAULT_DB_TIMEOUT)
      val sampleProg = readProgram(new File("src/test/resources/ProcessingTest/sample.vpr"))
      val sampleUS = UserSubmission(0,
        Timestamp.valueOf(LocalDateTime.now()),
        "sample.vpr",
        sampleProg,
        getLOC(sampleProg),
        "Silicon",
        Array("--timeout", "10"),
        "Silicon",
        true,
        1500
      )
      requests.post(host + "/submit-program", data = jsonifySubmission(sampleUS))
      val lockingProcess = Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline")
      lockingProcess.run
      Thread.sleep(100)
      assertThrows[GlobalLockException](getGlobalLock())
    } finally {
      dbProcess.destroy()
    }
  }*/

  /** IMPORTANT: This test will clear the database, do not run once actually in use */
  test("Pipeline integration test") {
    val dbProcess = Process("./run.sh").run
    Thread.sleep(1000) // let processes startup

    try {
      //clear database
      Await.ready(clearDB(), DEFAULT_DB_TIMEOUT)

      //two almost identical programs
      val sampleProg  = readProgram(new File("src/test/resources/ProcessingTest/sample.vpr"))
      val sampleProg2 = readProgram(new File("src/test/resources/ProcessingTest/sample2.vpr"))
      //very different program
      val sampleProg3 = readProgram(new File("src/test/resources/ProcessingTest/sample3.vpr"))
      val sampleUS = UserSubmission(
        0,
        Timestamp.valueOf(LocalDateTime.now()),
        sampleProg,
        getLOC(sampleProg),
        "Silicon",
        Array("--timeout", "10"),
        "Silicon",
        true,
        1500
      )
      val sampleUS2 = UserSubmission(
        0,
        Timestamp.valueOf(LocalDateTime.now()),
        sampleProg2,
        getLOC(sampleProg2),
        "Silicon",
        Array("--timeout", "20"),
        "Silicon",
        true,
        1500
      )
      val sampleUS3 = UserSubmission(
        0,
        Timestamp.valueOf(LocalDateTime.now()),
        sampleProg3,
        getLOC(sampleProg3),
        "Silicon",
        Array(),
        "Silicon",
        true,
        3000
      )

      requests.post(host + "/submit-program", data = jsonifySubmission(sampleUS))

      val usEntry = Await.result(getOldestUserSubmission(), DEFAULT_DB_TIMEOUT)
      //check returned entry is identical to stored entry
      usEntry match {
        case Some(entry) => {
          assert(sampleUS.program == entry.program)
          assert(sampleUS.loc == entry.loc)
          assert(sampleUS.originalVerifier == entry.originalVerifier)
          assert(sampleUS.frontend == entry.frontend)
          assert(sampleUS.success == entry.success)
          assert(sampleUS.runtime == entry.runtime)

          //check args array deserialized correctly
          val args = Array("--timeout", "10")
          assert(entry.args.sameElements(args))
        }
        case None => assert(false)
      }

      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline").!

      //check that UserSubmission was deleted and an Entry was created for each table
      assert(Await.result(getUSCount(), DEFAULT_DB_TIMEOUT) == 0)
      assert(Await.result(getPECount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getSRCount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getCRCount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getPPCount(), DEFAULT_DB_TIMEOUT) == 1)

      requests.post(host + "/submit-program", data = jsonifySubmission(sampleUS2))

      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline").!

      //program is similar, make sure no new entries
      assert(Await.result(getUSCount(), DEFAULT_DB_TIMEOUT) == 0)
      assert(Await.result(getPECount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getSRCount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getCRCount(), DEFAULT_DB_TIMEOUT) == 1)
      assert(Await.result(getPPCount(), DEFAULT_DB_TIMEOUT) == 1)

      requests.post(host + "/submit-program", data = jsonifySubmission(sampleUS3))
      Process(s"$SCALA_CLASS_BASH_FILE dataCollection.ProcessingPipeline").!

      //program is different, make sure not filtered out
      assert(Await.result(getUSCount(), DEFAULT_DB_TIMEOUT) == 0)
      assert(Await.result(getPECount(), DEFAULT_DB_TIMEOUT) == 2)
      assert(Await.result(getSRCount(), DEFAULT_DB_TIMEOUT) == 2)
      assert(Await.result(getCRCount(), DEFAULT_DB_TIMEOUT) == 2)
      assert(Await.result(getPPCount(), DEFAULT_DB_TIMEOUT) == 2)

      val patternMatches = PatternMatcher.matchRegexAgainstDatabase("while\\([^)]*\\)")
      //assert that the while( ) pattern was found on the 7th line of sampleUS
      assert(patternMatches.exists(pm => pm.matchIndices.contains(7)))
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

  def jsonifySubmission(us: UserSubmission): Obj = {
    Obj(
      "program"          -> us.program,
      "frontend"         -> us.frontend,
      "args"             -> Arr.from[String](us.args),
      "originalVerifier" -> us.originalVerifier,
      "success"          -> us.success,
      "runtime"          -> us.runtime
    )
  }
}
