package dataCollection

import database.{CarbonResult, DBQueryInterface, ProgramEntry, SiliconResult, UserSubmission}
import viper.silver.parser.FastParser
import database.ExecContext._

import java.io.{File, FileWriter}
import java.nio.file.Paths
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object ProcessingPipeline {
  private val fastParser = new FastParser()
  private val fPrinter = Fingerprinter

  def createProgramEntryFromSubmission(us: UserSubmission): ProgramEntry = {
    val tmpFile = createTempProgramFile(us.submissionId, us.program)
    val parsedProgram = fastParser.parse(us.program, Paths.get(tmpFile))
    val programPrint = fPrinter.fingerprintPProgram(parsedProgram)
    val parseSuccess = parsedProgram.errors.isEmpty
    val hasPreamble = parsedProgram.predicates.nonEmpty || parsedProgram.domains.nonEmpty || parsedProgram.fields.nonEmpty || parsedProgram.extensions.nonEmpty
    removeTempProgramFile(tmpFile)
    ProgramEntry(0,
      Timestamp.valueOf(LocalDateTime.now()),
      us.originalName,
      us.program,
      us.loc,
      us.frontend,
      us.originalVerifier,
      us.args,
      programPrint,
      parseSuccess,
      hasPreamble,
    )
  }

  def doesSimilarEntryExist(pe: ProgramEntry, sr: SiliconResult, cr: CarbonResult): Boolean = {
    val potMatches = DBQueryInterface.getPotentialMatchingEntries(pe)
    val foundMatch: Future[Boolean] = potMatches flatMap (
      seq => (seq map (otherPE => doEntriesMatch(pe, sr, cr, otherPE))).reduceLeft((a, b) => a flatMap (xa => b map (xb => xa || xb)))
      )
    Await.result(foundMatch, Duration.Inf)
  }

  private def doEntriesMatch(pe1: ProgramEntry, sr: SiliconResult, cr: CarbonResult, pe2: ProgramEntry)(implicit ec: ExecutionContext): Future[Boolean] = {
    if (pe1.isSimilarTo(pe2)) {
      val otherSilRes = DBQueryInterface.getLatestSilResForEntry(pe2.programEntryId)
      val otherCarbRes = DBQueryInterface.getLatestCarbResForEntry(pe2.programEntryId)
      val silMatch = otherSilRes map {
        case Some(silRes) => sr.isSimilarTo(silRes, 1.5)
        case None => false
      }
      val carbMatch = otherCarbRes map {
        case Some(carbRes) => cr.isSimilarTo(carbRes, 1.5)
        case None => false
      }
      for {
        b1 <- silMatch
        b2 <- carbMatch
      } yield b1 && b2
    } else {
      Future(false)
    }
  }


  def generateSiliconResults(pe: ProgramEntry): SiliconResult = {
    val runner = new CollectionSilFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile)
    // original arguments are only used if the program was also originally run with silicon
    if (pe.originalVerifier == "Silicon") {
      args = args ++ pe.args
    }
    runner.runMain(args)
    val runtime = runner.getTime
    val siliconHash = runner.siliconHash
    val phaseRuntimes = runner.getPhaseRuntimes
    val benchmarkResults = runner.getBenchmarkResults
    val success = runner.hasSucceeded
    val errors = runner.errors
    SiliconResult(0,
      Timestamp.valueOf(LocalDateTime.now()),
      siliconHash,
      pe.programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes,
      benchmarkResults
    )
  }

  def generateCarbonResults(pe: ProgramEntry): CarbonResult = {
    val runner = new CollectionCarbonFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile)
    // original arguments are only used if the program was also originally run with carbon
    if (pe.originalVerifier == "Carbon") {
      args = args ++ pe.args
    }
    runner.main(args)
    val runtime = runner.getTime
    val carbonHash = runner.carbonHash
    val phaseRuntimes = runner.getPhaseRuntimes
    val success = runner.hasSucceeded
    val errors = runner.errors
    CarbonResult(0,
      Timestamp.valueOf(LocalDateTime.now()),
      carbonHash,
      pe.programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes
    )
  }

  private def createTempProgramFile(id: Long, program: String): String = {
    val fName = s"./tmp/$id.vpr"
    val fw: FileWriter = new FileWriter(new File(fName))
    fw.write(program)
    fw.close()
    fName
  }

  private def removeTempProgramFile(fName: String): Unit = {
    val f = new File(fName)
    f.delete()
  }
}
