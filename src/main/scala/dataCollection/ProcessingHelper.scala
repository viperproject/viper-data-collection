package dataCollection

import dataCollection.customFrontends.{CollectionCarbonFrontend, CollectionSilFrontend, CollectionSiliconFrontend, VerifierFeature}
import database.{DBQueryInterface, ProgramPrintEntry}
import queryFrontend._
import viper.silver.parser.FastParser
import database.DBExecContext._
import util.Config._
import util._
import viper.silver.verifier.{AbstractError, Failure, Success, TimeoutOccurred}

import java.nio.file.Paths
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}

/** Provides functions to aid in the processing of submissions and generating database entries */
object ProcessingHelper {
  private val fastParser = new FastParser()
  private val fPrinter   = Fingerprinter

  /** Tries to get userSubmission with the oldest submissionDate and converts it to ProgramEntry, then deletes userSubmission from database */
  def processOldestSubmission(): Option[ProgramEntry] = {
    val submissionOpt = Await.result(DBQueryInterface.getOldestUserSubmission(), DEFAULT_DB_TIMEOUT)
    submissionOpt match {
      case Some(submission) =>
        val progEntry = createProgramEntryFromSubmission(submission)
        Await.result(DBQueryInterface.deleteUserSubmission(submission.submissionId), DEFAULT_DB_TIMEOUT)
        Some(progEntry)
      case None => None
    }
  }

  def createProgramPrintEntry(program: String): ProgramPrintEntry = {
    val tmpFile = createTempProgramFile(program.hashCode, program)

    val parsedProgram = fastParser.parse(program, Paths.get(tmpFile))
    val programPrint  = fPrinter.fingerprintPProgram(parsedProgram)

    removeTempProgramFile(tmpFile)

    ProgramPrintEntry(0, 0, programPrint)
  }

  def createProgramEntryFromSubmission(us: UserSubmission): ProgramEntry = {
    val tmpFile = createTempProgramFile(us.submissionId, us.program)

    val parsedProgram = fastParser.parse(us.program, Paths.get(tmpFile))
    val parseSuccess  = parsedProgram.errors.isEmpty

    removeTempProgramFile(tmpFile)

    ProgramEntry(
      0,
      Timestamp.valueOf(LocalDateTime.now()),
      us.program,
      us.loc,
      us.frontend,
      us.originalVerifier,
      us.args,
      us.runtime,
      parseSuccess
    )
  }

  /** Class that stores a boolean to indicate whether an event occurred. Usage example: Passed into a closure in [[existsSimilarEntry]]
    * to avoid unnecessary computations once match has been found.
    */
  private class BooleanCarrier(var bool: Boolean = false)

  /** Searches database for ProgramEntries that could be a match, then compares the entry, newest siliconResult,
    * CarbonResult and ProgramPrint for each to see if they are too similar. This is done using a DatabasePublisher
    * to avoid loading all Entries into memory at once.
    *
    * @return true if there is a database entry that is too similar to the argument
    */
  def existsSimilarEntry(et: ProgramTuple): Boolean = {
    val potMatches = DBQueryInterface.getPotentialMatchingEntryTuples(et.programEntry)
    val matchState = new BooleanCarrier()
    val matchResults = potMatches.mapResult(otherEntry => {
      if (matchState.bool) true
      else {
        val dm = doEntriesMatch(et, otherEntry)
        matchState.bool = dm
        dm
      }
    })
    var foundMatch = false
    val evaluation = matchResults.foreach(r => foundMatch = foundMatch || r)
    Await.ready(evaluation, LONG_TIMEOUT)
    foundMatch
  }

  /** * Searches Database for potential matching ProgramEntries, matches only entry and ProgramPrint. This method is used
    * instead of [[existsSimilarEntry]] in case [[STORE_ONLY]] is enabled.
    * @return True if a program with a similar entry and ProgramPrint is already stored.
    */
  def existsSimilarProgram(pe: ProgramEntry, pp: ProgramPrint): Boolean = {
    val potMatches = DBQueryInterface.getPotentialMatchingPrograms(pe)
    val matchState = new BooleanCarrier()
    val matchResults = potMatches.mapResult(otherEntry => {
      if (matchState.bool) true
      else {
        lazy val peMatch = pe.isSimilarTo(otherEntry._1)
        lazy val ppMatch = doProgramPrintsMatch(pp, otherEntry._2.programPrint, pe.frontend)
        val dm           = peMatch && ppMatch
        matchState.bool = dm
        dm
      }
    })
    var foundMatch = false
    val evaluation = matchResults.foreach(r => foundMatch = foundMatch || r)
    Await.ready(evaluation, LONG_TIMEOUT)
    foundMatch
  }

  /** Compares one EntryTuple to another, taking into consideration their [[VerResult]]s, [[ProgramPrint]]s and [[ProgramEntry]]s
    *
    * @return true if the entries are too similar
    */
  private def doEntriesMatch(et1: ProgramTuple, et2: ProgramTuple): Boolean = {
    lazy val peMatch     = et1.programEntry.isSimilarTo(et2.programEntry)
    lazy val srMatch     = et1.siliconResult.isSimilarTo(et2.siliconResult)
    lazy val crMatch     = et1.carbonResult.isSimilarTo(et2.carbonResult)
    lazy val pprint1     = et1.programPrintEntry.programPrint
    lazy val pprint2     = et2.programPrintEntry.programPrint
    lazy val pprintMatch = doProgramPrintsMatch(pprint1, pprint2, et1.programEntry.frontend)

    peMatch && srMatch && crMatch && pprintMatch
  }

  /** Decides whether two [[ProgramPrint]]s are similar enough to count as a match. If the programs come from a frontend,
    * only their functions and methods are compared, since a lot of frontends generate a very similar preamble. The programs
    * count as similar if they have the same number of methods and functions and their nodes match at least [[VIPER_MATCH_THRESHOLD]]
    * or [[FRONTEND_MATCH_THRESHOLD]], or if one program is entirely contained in the other.
    */
  def doProgramPrintsMatch(pprint1: ProgramPrint, pprint2: ProgramPrint, frontend: String): Boolean = {
    lazy val cpprint1 = new ComparableProgramPrint(pprint1)
    lazy val cpprint2 = new ComparableProgramPrint(pprint2)
    lazy val sameNumMethFunc =
      cpprint1.numMethods == cpprint2.numMethods && cpprint1.numFunctions == cpprint2.numFunctions
    lazy val matchResult1 = cpprint1 matchTrees cpprint2
    lazy val matchResult2 = cpprint2 matchTrees cpprint1
    lazy val isSubset     = matchResult1.isSubset || matchResult2.isSubset
    lazy val similarTrees = if (frontend == "Silicon" || frontend == "Carbon") {
      matchResult1.isViperMatch && matchResult2.isViperMatch
    } else {
      matchResult1.isFrontendMatch && matchResult2.isFrontendMatch
    }
    (sameNumMethFunc && similarTrees) || isSubset
  }

  /** Takes the ID of an existing programEntry, creates a SiliconResult for this entry and inserts it into the SiliconResults table */
  def silBenchmarkProgramEntry(programEntryId: Long): Unit = {
    val entryOpt = Await.result(DBQueryInterface.getProgramEntryByID(programEntryId), DEFAULT_DB_TIMEOUT)
    entryOpt match {
      case Some(entry) =>
        val runtimeLimit     = ((entry.originalRuntime * BENCHMARK_TIMEOUT_MULTIPLIER) / 1000).toInt
        val (silRes, vfeats) = generateSiliconResults(entry, timeOutSeconds = runtimeLimit)
        val silResId         = Await.result(DBQueryInterface.insertSiliconResult(silRes), DEFAULT_DB_TIMEOUT)
        Await.ready(
          DBQueryInterface.insertVerifierFeatures("Silicon", silResId, programEntryId, vfeats),
          DEFAULT_DB_TIMEOUT
        )
      case None => println("ID does not match any stored program")
    }
  }

  /** Takes the ID of an existing programEntry, creates a CarbonResult for this entry and inserts it into the CarbonResult table */
  def carbBenchmarkProgramEntry(programEntryId: Long): Unit = {
    val entryOpt = Await.result(DBQueryInterface.getProgramEntryByID(programEntryId), DEFAULT_DB_TIMEOUT)
    entryOpt match {
      case Some(entry) =>
        val runtimeLimit      = ((entry.originalRuntime * BENCHMARK_TIMEOUT_MULTIPLIER) / 1000).toInt
        val (carbRes, vfeats) = generateCarbonResults(entry, timeOutSeconds = runtimeLimit)
        val carbResId         = Await.result(DBQueryInterface.insertCarbonResult(carbRes), DEFAULT_DB_TIMEOUT)
        Await.ready(
          DBQueryInterface.insertVerifierFeatures("Carbon", carbResId, programEntryId, vfeats),
          DEFAULT_DB_TIMEOUT
        )
      case None => println("ID does not match any stored program")
    }
  }

  /** function type that takes a ProgramEntry, a list of arguments, and seconds to timeout and returns the results
    * of this program's verification
    */
  type verifierResultFunction = (ProgramEntry, Array[String], Int) => (VerResult, Seq[VerifierFeature])

  /** @param runner An instantiated [[CollectionSilFrontend]] which will be used to verify the program
    * @param pe the [[ProgramEntry]] for the program which will be verified
    * @param args the arguments which will be passed to the verifier, the first should be the location of the program file
    * @return The result and features generated by the [[CollectionSilFrontend]]
    */
  private def generateVerifierResults(
    runner: CollectionSilFrontend,
    pe: ProgramEntry,
    args: Array[String]
  ): (VerResult, Seq[VerifierFeature]) = {
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    runner.main(Array(tmpFile) ++ args)

    val verRes = VerResult(
      0,
      Timestamp.valueOf(LocalDateTime.now()),
      runner.verifierHash,
      pe.programEntryId,
      runner.hasSucceeded,
      runner.errors.exists(ae => ae.isInstanceOf[TimeoutOccurred]),
      runner.getTime,
      runner.getVerificationResult match {
        case Some(value) =>
          value match {
            case Success         => Array[VerError]()
            case Failure(errors) => errors.toArray map abstractToVerError
          }
        case None => Array[VerError]()
      },
      runner.getPhaseRuntimes.toArray
    )

    removeTempProgramFile(tmpFile)
    (verRes, runner.getFeatures)
  }

  /** @param pe            ProgramEntry for which to get the results of verifying it through Silicon
    * @param extraArgs      arguments that will be passed into Silicon alongside the original ones
    * @param timeOutSeconds how many seconds until Silicon should terminate, 0 => no timeout
    */
  def generateSiliconResults(
    pe: ProgramEntry,
    extraArgs: Array[String] = Array(),
    timeOutSeconds: Int = 0
  ): (VerResult, Seq[VerifierFeature]) = {
    val runner = new CollectionSiliconFrontend

    var args: Array[String] = extraArgs
    if (pe.originalVerifier == "Silicon") {
      args = args ++ pe.args
    }
    args = filterSiliconArgs(args)
    args ++= Array("--timeout", timeOutSeconds.toString)

    generateVerifierResults(runner, pe, args)
  }

  /** Removes any arguments for Silicon which are only meaningful locally or should not be used during benchmarking */
  private def filterSiliconArgs(args: Array[String]): Array[String] = {
    var filteredArgs = args
    val toFilter = Array(
      "--timeout",
      "--cvc5Exe",
      "--enableTempDirectory",
      "--mapAxiomatizationFile",
      "--multisetAxiomatizationFile",
      "--printMethodCFGs",
      "--proverLogFile",
      "--sequenceAxiomatizationFile",
      "--setAxiomatizationFile",
      "--submitForEvaluation",
      "--tempDirectory",
      "--z3Exe",
      "--z3LogFile"
    )
    for (filter <- toFilter) {
      filteredArgs = filterArgs(filteredArgs, filter)
    }
    filteredArgs
  }

  /** @param pe            ProgramEntry for which to get the results of verifying it through Carbon
    * @param extraArgs      arguments that will be passed into Carbon alongside the original ones
    * @param timeOutSeconds how many seconds until Silicon should terminate, 0 => no timeout
    */
  def generateCarbonResults(
    pe: ProgramEntry,
    extraArgs: Array[String] = Array(),
    timeOutSeconds: Int = 0
  ): (VerResult, Seq[VerifierFeature]) = {
    val runner = new CollectionCarbonFrontend(timeOutSeconds)

    var args: Array[String] = extraArgs
    if (pe.originalVerifier == "Carbon") {
      args = args ++ pe.args
    }

    args = filterCarbonArgs(args)

    generateVerifierResults(runner, pe, args)
  }

  /** Removes any arguments for Carbon which are only meaningful locally or should not be used during benchmarking */
  private def filterCarbonArgs(args: Array[String]): Array[String] = {
    var filteredArgs = args
    val toFilter = Array(
      "--timeout",
      "--boogieExe",
      "--proverLog",
      "--z3Exe"
    )
    for (filter <- toFilter) {
      filteredArgs = filterArgs(filteredArgs, filter)
    }
    filteredArgs
  }

  /** if [[toFilter]] is found in [[args]], drops that and next index in the array, if next index is not another flag */
  private def filterArgs(args: Array[String], toFilter: String): Array[String] = {
    val argInd = args.indexOf(toFilter)
    if (argInd == -1) {
      args
    } else {
      if (argInd < args.length - 1 && args(argInd + 1).startsWith("--")) {
        args.dropRight(args.length - argInd) ++ args.drop(argInd + 1)
      } else {
        args.dropRight(args.length - argInd) ++ args.drop(argInd + 2)
      }
    }
  }

  def abstractToVerError(ae: AbstractError): VerError = {
    VerError(ae.fullId, ae.readableMessage)
  }

}
