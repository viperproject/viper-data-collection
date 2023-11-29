package dataCollection

import dataCollection.customFrontends.{CollectionCarbonFrontend, CollectionSilFrontend, CollectionSiliconFrontend, VerifierFeature}
import database.{DBQueryInterface, ProgramEntry, ProgramPrintEntry, UserSubmission, VerError, VerResult}
import viper.silver.parser.{FastParser, PProgram}
import database.DBExecContext._
import util.Config._
import util._
import viper.silver.verifier.{Failure, Success}

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
      us.originalName,
      us.program,
      us.loc,
      us.frontend,
      us.originalVerifier,
      us.args,
      us.runtime,
      parseSuccess
    )
  }

  /** Searches the database for programs with the same or similar metadata as the entry. Returns true if at least a third of the entry's
    * features are present in less than half of all entries.
    */
  def shouldDropByMetadata(et: ProgramTuple): Boolean = {
    val totalEntries = DBQueryInterface.getPECount()
    val totalSilRes  = DBQueryInterface.getUniqueSRCount()
    val totalCarbRes = DBQueryInterface.getUniqueCRCount()

    val sameFrontend     = DBQueryInterface.getFrontendCount(et.programEntry.frontend)
    val sameVerifier     = DBQueryInterface.getVerifierCount(et.programEntry.originalVerifier)
    val sameParseSuccess = DBQueryInterface.getParseSuccessCount(et.programEntry.parseSuccess)
    val similarLOC       = DBQueryInterface.getLocRangeCount(et.programEntry.loc - 100, et.programEntry.loc + 100)
    val sameSilSuccess   = DBQueryInterface.getSilSuccessCount(et.siliconResult.success)
    val similarSilRuntime = DBQueryInterface.getSiliconRuntimeRangeCount(
      (et.siliconResult.runtime / 1.5).toLong,
      (et.siliconResult.runtime * 1.5).toLong
    )
    val sameCarbSuccess = DBQueryInterface.getCarbonSuccessCount(et.carbonResult.success)
    val similarCarbRuntime = DBQueryInterface.getCarbonRuntimeRangeCount(
      (et.carbonResult.runtime / 1.5).toLong,
      (et.carbonResult.runtime * 1.5).toLong
    )

    val programFeaturePercentages = Seq(sameFrontend, sameVerifier, sameParseSuccess, similarLOC) map
      (fc => fc flatMap (c => totalEntries map (t => c.toDouble / t)))
    val siliconFeaturePercentages = Seq(sameSilSuccess, similarSilRuntime) map
      (fc => fc flatMap (c => totalSilRes map (t => c.toDouble / t)))
    val carbonFeaturePercentages = Seq(sameCarbSuccess, similarCarbRuntime) map
      (fc => fc flatMap (c => totalCarbRes map (t => c.toDouble / t)))

    val allPercentages = Await.result(
      Future.sequence(programFeaturePercentages ++ siliconFeaturePercentages ++ carbonFeaturePercentages),
      DEFAULT_DB_TIMEOUT
    )
    val shouldDrop = !(allPercentages.count(p =>
      p <= METADATA_FILTER_THRESHOLD
    ) >= (allPercentages.length * METADATA_AMOUNT_THRESHOLD).toInt)
    shouldDrop
  }

  def shouldDropByFeatures(feats: Seq[VerifierFeature]): Boolean = {
    val totalEntries = Await.result(DBQueryInterface.getPECount(), DEFAULT_DB_TIMEOUT)

    val relevantFeats = feats.filter(_.useForFiltering)
    val featOccurrences = relevantFeats map { f =>
      DBQueryInterface.getProgramWithFeatureValueCount(f.name, f.value)
    }
    val featOccurrenceFraction = featOccurrences map { fc =>
      fc map (c => c.toDouble / totalEntries)
    }
    val fractions = Await.result(Future.sequence(featOccurrenceFraction), DEFAULT_DB_TIMEOUT)
    (fractions.sum / fractions.length) <= FEATURE_FILTER_THRESHOLD
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
    Await.ready(evaluation, Duration(200, SECONDS))
    foundMatch
  }

  /** Compares one EntryTuple to another
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
        Await.ready(DBQueryInterface.insertVerifierFeatures("Silicon", silResId, vfeats), DEFAULT_DB_TIMEOUT)
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
        Await.ready(DBQueryInterface.insertVerifierFeatures("Carbon", carbResId, vfeats), DEFAULT_DB_TIMEOUT)
      case None => println("ID does not match any stored program")
    }
  }

  /** function type that takes a ProgramEntry, a list of arguments, and seconds to timeout and returns the results
    * of this program's verification
    */
  type verifierResultFunction = (ProgramEntry, Array[String], Int) => (VerResult, Seq[VerifierFeature])

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
      runner.getTime,
      runner.getVerificationResult match {
        case Some(value) =>
          value match {
            case Success         => Array[VerError]()
            case Failure(errors) => errors.toArray map VerError.toError
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
    args = filterArgs(args, "--timeout")
    args ++= Array("--timeout", timeOutSeconds.toString)

    generateVerifierResults(runner, pe, args)
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

    generateVerifierResults(runner, pe, args)
  }

  /** if [[toFilter]] is found in [[args]], drops that and next index in the array */
  private def filterArgs(args: Array[String], toFilter: String): Array[String] = {
    val argInd = args.indexOf(toFilter)
    if (argInd == -1) {
      args
    } else {
      args.dropRight(args.length - argInd) ++ args.drop(argInd + 2)
    }
  }

}
