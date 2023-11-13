package dataCollection

import database.{CarbonResult, DBQueryInterface, ProgramEntry, ProgramPrintEntry, SiliconResult, UserSubmission}
import viper.silver.parser.FastParser
import database.ExecContext._

import java.io.{File, FileWriter}
import java.nio.file.Paths
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Provides functions to aid in the processing of submissions and generating database entries */
object ProcessingHelper {
  private val fastParser = new FastParser()
  private val fPrinter = Fingerprinter

  /** Tries to get userSubmission with the oldest submissionDate and converts it to ProgramEntry */
  def processOldestSubmission(): Option[ProgramEntry] = {
    val submissionOpt = Await.result(DBQueryInterface.getOldestUserSubmission(), Duration.Inf)
    submissionOpt match {
      case Some(submission) => {
        val progEntry = createProgramEntryFromSubmission(submission)
        Await.result(DBQueryInterface.deleteUserSubmission(submission.submissionId), Duration.Inf)
        Some(progEntry)
      }
      case None => None
    }
  }

  def createProgramPrintEntry(program: String): ProgramPrintEntry = {
    val tmpFile = createTempProgramFile(program.hashCode, program)
    val parsedProgram = fastParser.parse(program, Paths.get(tmpFile))
    val programPrint = fPrinter.fingerprintPProgram(parsedProgram)
    removeTempProgramFile(tmpFile)
    ProgramPrintEntry(0,
      0,
      programPrint)
  }

  def createProgramEntryFromSubmission(us: UserSubmission): ProgramEntry = {
    val tmpFile = createTempProgramFile(us.submissionId, us.program)
    val parsedProgram = fastParser.parse(us.program, Paths.get(tmpFile))
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
      parseSuccess,
      hasPreamble,
    )
  }

  /** Searches database for ProgramEntries that could be a match, then compares the entry, newest siliconResult
   * and CarbonResult for each to see if they are too similar
   *
   * @return true if there is a database entry that is too similar to the argument */
  def existsSimilarEntry(et: EntryTuple): Boolean = {
    val potMatches = DBQueryInterface.getPotentialMatchingEntryTuples(et.programEntry)
    val foundMatch: Future[Boolean] = potMatches flatMap (
      // Map doEntriesMatch on every potential entry, then reduce the results by or-ing them
      seq => (seq map (otherET => doEntriesMatch(et, otherET)))
        .reduceLeft((fBool1, fBool2) => fBool1 flatMap (bool1 => fBool2 map (bool2 => bool1 || bool2)))
      )
    Await.result(foundMatch, Duration.Inf)
  }

  /** Compares one programEntry, siliconResult and CarbonResult to another
   *
   * @return true if the entries are too similar, packed in Future to multi-thread comparisons */
  private def doEntriesMatch(et1: EntryTuple, et2: EntryTuple)(implicit ec: ExecutionContext): Future[Boolean] = {
    Future {
      lazy val peMatch = et1.programEntry.isSimilarTo(et2.programEntry)
      lazy val srMatch = et1.siliconResult.isSimilarTo(et2.siliconResult)
      lazy val crMatch = et1.carbonResult.isSimilarTo(et2.carbonResult)
      lazy val pprint1 = et1.programPrintEntry.programPrint
      lazy val pprint2 = et2.programPrintEntry.programPrint
      lazy val pprintMatch = doProgramPrintsMatch(pprint1, pprint2, et1.programEntry.frontend)

      peMatch && srMatch && crMatch && pprintMatch
    }
  }

  def doProgramPrintsMatch(pprint1: ProgramPrint, pprint2: ProgramPrint, frontend: String): Boolean = {
    lazy val sameNumMethFunc = pprint1.numMethods == pprint2.numMethods && pprint1.numFunctions == pprint2.numFunctions
    lazy val matchResult1 = pprint1.matchTrees(pprint2)
    lazy val matchResult2 = pprint2.matchTrees(pprint1)
    lazy val isSubset = matchResult1.isSubset || matchResult2.isSubset
    lazy val similarTrees = if (frontend == "Silicon" || frontend == "Carbon") {
      matchResult1.isViperMatch && matchResult2.isViperMatch
    } else {
      matchResult1.isFrontendMatch && matchResult2.isFrontendMatch
    }
    sameNumMethFunc && (similarTrees || isSubset)
  }

  /** Takes the ID of an existing programEntry, creates a SiliconResult for this entry and inserts it into the SiliconResults table */
  def silBenchmarkProgramEntry(programEntryId: Long): Unit = {
    val entryOpt = Await.result(DBQueryInterface.getProgramEntryByID(programEntryId), Duration.Inf)
    entryOpt match {
      case Some(entry) =>
        val silRes = generateSiliconResults(entry)
        Await.result(DBQueryInterface.insertSiliconResult(silRes), Duration.Inf)
      case None => println("ID does not match any stored program")
    }
  }

  /** Takes the ID of an existing programEntry, creates a CarbonResult for this entry and inserts it into the CarbonResult table */
  def carbBenchmarkProgramEntry(programEntryId: Long): Unit = {
    val entryOpt = Await.result(DBQueryInterface.getProgramEntryByID(programEntryId), Duration.Inf)
    entryOpt match {
      case Some(entry) =>
        val carbRes = generateCarbonResults(entry)
        Await.result(DBQueryInterface.insertCarbonResult(carbRes), Duration.Inf)
      case None => println("ID does not match any stored program")
    }
  }


  /** @param pe       ProgramEntry for which to get the results of verifying it through Silicon
   * @param extraArgs arguments that will be passed into Silicon alongside the original ones */
  def generateSiliconResults(pe: ProgramEntry, extraArgs: Array[String] = Array()): SiliconResult = {
    val runner = new CollectionSilFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile) ++ extraArgs
    // original arguments are only used if the program was also originally run with silicon
    if (pe.originalVerifier == "Silicon") {
      args = args ++ pe.args
    }
    runner.runMain(args)
    val runtime = runner.getTime
    val siliconHash = runner.siliconHash
    val phaseRuntimes = runner.getPhaseRuntimes.toArray
    val benchmarkResults = runner.getBenchmarkResults.toArray
    val success = runner.hasSucceeded
    val errors = runner.errors.toArray

    removeTempProgramFile(tmpFile)

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

  /** @param pe       ProgramEntry for which to get the results of verifying it through Carbon
   * @param extraArgs arguments that will be passed into Carbon alongside the original ones */
  def generateCarbonResults(pe: ProgramEntry, extraArgs: Array[String] = Array()): CarbonResult = {
    val runner = new CollectionCarbonFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile) ++ extraArgs
    // original arguments are only used if the program was also originally run with carbon
    if (pe.originalVerifier == "Carbon") {
      args = args ++ pe.args
    }
    runner.main(args)
    val runtime = runner.getTime
    val carbonHash = runner.carbonHash
    val phaseRuntimes = runner.getPhaseRuntimes.toArray
    val success = runner.hasSucceeded
    val errors = runner.errors.toArray

    removeTempProgramFile(tmpFile)

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

  /** Verifiers and Parsers need a local file that contains the program, this function creates such a temporary file and returns the path */
  def createTempProgramFile(id: Long, program: String): String = {
    val fName = s"./tmp/$id.vpr"
    val fw: FileWriter = new FileWriter(new File(fName))
    fw.write(program)
    fw.close()
    fName
  }

  /** Removes the temporary program file */
  def removeTempProgramFile(fName: String): Unit = {
    val f = new File(fName)
    f.delete()
  }
}

case class EntryTuple(programEntry: ProgramEntry,
                      programPrintEntry: ProgramPrintEntry,
                      siliconResult: SiliconResult,
                      carbonResult: CarbonResult)
