package dataCollection

import database.UserSubmission
import org.scalatest.funsuite.AnyFunSuite
import util.getLOC

import java.io.File
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source.fromFile

class ProcessingTest extends AnyFunSuite {

  import database.DBQueryInterface._

  test("Pipeline integration test") {
    Await.ready(clearDB(), Duration.Inf)

    //two almost identical programs
    val sampleProg = readProgram(new File("src/test/resources/ProcessingTest/sample.vpr"))
    val sampleProg2 = readProgram(new File("src/test/resources/ProcessingTest/sample2.vpr"))
    //very different program
    val sampleProg3 = readProgram(new File("src/test/resources/ProcessingTest/sample3.vpr"))
    val sampleUS = UserSubmission(0,
      Timestamp.valueOf(LocalDateTime.now()),
      "sample.vpr",
      sampleProg,
      getLOC(sampleProg),
      "Silicon",
      Array(),
      "Silicon",
      true,
      1500
    )
    val sampleUS2 = UserSubmission(0,
      Timestamp.valueOf(LocalDateTime.now()),
      "sample2.vpr",
      sampleProg2,
      getLOC(sampleProg2),
      "Silicon",
      Array(),
      "Silicon",
      true,
      1500
    )
    val sampleUS3 = UserSubmission(0,
      Timestamp.valueOf(LocalDateTime.now()),
      "sample3.vpr",
      sampleProg3,
      getLOC(sampleProg3),
      "Silicon",
      Array(),
      "Silicon",
      true,
      3000
    )
    insertUserSubmission(sampleUS)
    val usEntry = Await.result(getOldestUserSubmission(), Duration.Inf)
    //check returned entry is identical to stored entry
    usEntry match {
      case Some(entry) => {
        assert(sampleUS.submissionDate == entry.submissionDate)
        assert(sampleUS.originalName == entry.originalName)
        assert(sampleUS.program == entry.program)
        assert(sampleUS.loc == entry.loc)
        assert(sampleUS.originalVerifier == entry.originalVerifier)
        assert(sampleUS.frontend == entry.frontend)
        assert(sampleUS.success == entry.success)
        assert(sampleUS.runtime == entry.runtime)
      }
      case None => assert(false)
    }

    ProcessingPipeline.main(Array())

    //check that UserSubmission was deleted and an Entry was created for each table
    assert(Await.result(getUSCount(), Duration.Inf) == 0)
    assert(Await.result(getPECount(), Duration.Inf) == 1)
    assert(Await.result(getSRCount(), Duration.Inf) == 1)
    assert(Await.result(getCRCount(), Duration.Inf) == 1)
    assert(Await.result(getPPCount(), Duration.Inf) == 1)

    insertUserSubmission(sampleUS2)
    ProcessingPipeline.main(Array())

    //program is similar, make sure no new entries
    assert(Await.result(getUSCount(), Duration.Inf) == 0)
    assert(Await.result(getPECount(), Duration.Inf) == 1)
    assert(Await.result(getSRCount(), Duration.Inf) == 1)
    assert(Await.result(getCRCount(), Duration.Inf) == 1)
    assert(Await.result(getPPCount(), Duration.Inf) == 1)

    insertUserSubmission(sampleUS3)
    ProcessingPipeline.main(Array())

    //program is different, make sure not filtered out
    assert(Await.result(getUSCount(), Duration.Inf) == 0)
    assert(Await.result(getPECount(), Duration.Inf) == 2)
    assert(Await.result(getSRCount(), Duration.Inf) == 2)
    assert(Await.result(getCRCount(), Duration.Inf) == 2)
    assert(Await.result(getPPCount(), Duration.Inf) == 2)
  }

  def readProgram(file: File): String = {
    val fBuffer = fromFile(file)
    val prog = try fBuffer.mkString finally fBuffer.close()
    prog
  }
}
