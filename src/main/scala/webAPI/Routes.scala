package webAPI

import database.DBQueryInterface
import JSONReadWriters._
import queryFrontend.{FeatureEntry, PatternMatchResult, ProgramEntry, UserSubmission, VerResult}
import queryFrontend.JSONReadWriters._
import cask.Response
import database.tools.{PatternMatcher, VersionBenchmarkHelper}
import ujson.{Arr, Obj}
import util._
import util.Config._
import upickle.default._

import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.sys.process.Process

/** This object defines HTTP endpoints to retrieve information from the Database.
  *
  * Things to note if you want to add more endpoints:
  *
  * - Cask requires method annotation to define endpoints, you can either set a GET endpoint @cask.get(path) or a POST endpoint
  * using @cask.post(path) or @cask.postJson(path)
  *
  * - I would recommend using postJson instead of get, since get method parameters have to be passed with ?param=someParam in
  * the URL. If you use postJson, Cask automatically tries to parse the method parameters from the JSON that was sent along the request
  * (The JSON fields have to have the same name as the method parameter!).
  *
  * - To serialize your response data, you can either use the read, write functions from [[upickle]],
  * or define your own JSON object using [[ujson]]
  *
  * - If you need non-primitive JSON parsers, you can define them in [[JSONReadWriters]]
  *
  * - If you need to query the database, there are a lot of predefined queries in [[DBQueryInterface]]
  */
object Routes extends cask.MainRoutes {
  override val port: Int = WEBSERVER_LOCAL_PORT

  @cask.get("/")
  def default() = {
    "This is an API for the viper-data-collection database"
  }

  /** Returns all programEntries from the database that match the given metadata. */
  @cask.postJson("/program-entries-by-meta-data")
  def programEntriesByMetadata(
    earliestDate: Long,
    latestDate: Long,
    minLOC: Int,
    maxLOC: Int,
    frontend: Option[String],
    verifier: Option[String],
    parseSuccess: Option[Boolean]
  ): Response[Obj] = {
    try {
      val entries = Await.result(
        DBQueryInterface
          .getEntriesByMetadata(
            new Timestamp(earliestDate),
            new Timestamp(latestDate),
            minLOC,
            maxLOC,
            frontend,
            verifier,
            parseSuccess
          ),
        DEFAULT_DB_TIMEOUT
      )
      val responseObj = Obj("programEntries" -> Arr.from[ProgramEntry](entries)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns all programEntries from database whose programEntryId is contained in entryIds */
  @cask.postJson("/program-entries-by-ids")
  def programEntriesByIds(entryIds: Seq[Long]): Response[Obj] = {
    try {
      val entries     = Await.result(DBQueryInterface.getProgramEntriesByIDs(entryIds), DEFAULT_DB_TIMEOUT)
      val responseObj = Obj("programEntries" -> Arr.from[ProgramEntry](entries)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns a list of ids for all programEntries that have a [[VerifierResult]] which produced [[feature]] with the value of [[value]] */
  @cask.get("/program-ids-by-feature-value")
  def programIdsByFeatureValue(feature: String, value: String): Response[Obj] = {
    try {
      val peIds       = Await.result(DBQueryInterface.getPEIdsWithFeatureValue(feature, value), DEFAULT_DB_TIMEOUT)
      val responseObj = Obj("programIds" -> Arr.from[Long](peIds))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns programEntryIds of programEntries that have a SiliconResult with the given metadata */
  @cask.postJson("program-ids-by-silicon-data")
  def programIdsBySiliconData(
    success: Boolean,
    didTimeout: Boolean,
    minRuntime: Long,
    maxRuntime: Long
  ): Response[Obj] = {
    try {
      val peIds = Await.result(
        DBQueryInterface.getPEIdsBySiliconData(success, didTimeout, minRuntime, maxRuntime),
        DEFAULT_DB_TIMEOUT
      )
      val responseObj = Obj("programIds" -> Arr.from[Long](peIds))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns programEntryIds of programEntries that have a CarbonResult with the given metadata */
  @cask.postJson("program-ids-by-carbon-data")
  def programIdsByCarbonData(
    success: Boolean,
    didTimeout: Boolean,
    minRuntime: Long,
    maxRuntime: Long
  ): Response[Obj] = {
    try {
      val peIds = Await.result(
        DBQueryInterface.getPEIdsByCarbonData(success, didTimeout, minRuntime, maxRuntime),
        DEFAULT_DB_TIMEOUT
      )
      val responseObj = Obj("programIds" -> Arr.from[Long](peIds))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns a list of all feature values generated by all Silicon and Carbon results for this program */
  @cask.get("/feature-values-by-program-id")
  def featureValuesByProgramId(entryId: Long): Response[Obj] = {
    try {
      val results     = Await.result(DBQueryInterface.getFeaturesById(entryId), DEFAULT_DB_TIMEOUT)
      val responseObj = Obj("featureEntries" -> Arr.from[FeatureEntry](results)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns all Silicon [[VerifierResult]]s for the given programEntryIds. This is a 1:many relationship */
  @cask.postJson("/silicon-results-by-ids")
  def siliconResultsByIds(entryIds: Seq[Long]): Response[Obj] = {
    try {
      val results     = Await.result(DBQueryInterface.getSiliconResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val responseObj = Obj("siliconResults" -> Arr.from[VerResult](results)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns all Carbon [[VerifierResult]]s for the given programEntryIds. This is a 1:many relationship */
  @cask.postJson("/carbon-results-by-ids")
  def carbonResultsByIds(entryIds: Seq[Long]): Response[Obj] = {
    try {
      val results     = Await.result(DBQueryInterface.getCarbonResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val responseObj = Obj("carbonResults" -> Arr.from[VerResult](results)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Starts a separate Process running [[database.tools.SilVersionBenchmarker]] with the given hash */
  @cask.postJson("/benchmark-silicon-version")
  def benchmarkSiliconVersion(versionHash: String): Response[String] = {
    try {
      Process(s"$SCALA_CLASS_BASH_FILE database.tools.SilVersionBenchmarker $versionHash").run
      cask.Response(data = "Benchmarking started", statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  /** Starts a separate Process running [[database.tools.CarbVersionBenchmarker]] with the given hash */
  @cask.postJson("/benchmark-carbon-version")
  def benchmarkCarbonVersion(versionHash: String): Response[String] = {
    try {
      Process(s"$SCALA_CLASS_BASH_FILE database.tools.CarbVersionBenchmarker $versionHash").run
      cask.Response(data = "Benchmarking started", statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  /** Returns a [[queryFrontend.VerVersionDifferenceSummary]] summarizing differences between SiliconResults of the two given
    * Silicon versions. Only results that are already in the database are taken into account, no new ones are generated.
    */
  @cask.get("/silicon-version-difference")
  def siliconVersionDifference(versionHash1: String, versionHash2: String): Response[Obj] = {
    try {
      val result      = VersionBenchmarkHelper.generateSilVersionDiffSummary(versionHash1, versionHash2)
      val responseObj = Obj("verVersionDifferenceSummary" -> writeJs(result))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns a [[queryFrontend.VerVersionDifferenceSummary]] summarizing differences between CarbonResults of the two given
    * Carbon versions. Only results that are already in the database are taken into account, no new ones are generated.
    */
  @cask.get("/carbon-version-difference")
  def carbonVersionDifference(versionHash1: String, versionHash2: String): Response[Obj] = {
    try {
      val result      = VersionBenchmarkHelper.generateCarbVersionDiffSummary(versionHash1, versionHash2)
      val responseObj = Obj("verVersionDifferenceSummary" -> writeJs(result))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** Returns a list of (Frontend, Count) tuples for the given programEntryIds */
  @cask.postJson("/frontend-count-by-ids")
  def frontendCountByIds(entryIds: Seq[Long]): Response[Obj] = {
    try {
      val frontendCounts = Await.result(DBQueryInterface.getFrontendCountByIds(entryIds), DEFAULT_DB_TIMEOUT)
      val responseObj    = Obj("frontendCounts" -> Arr.from[(String, Int)](frontendCounts)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** @param regex regex string to match database programs against, should not include flags, i.e. "regex", not "/regex/gmi"
    * @param flags  flags from [[java.util.regex.Pattern]] or-ed together
    *
    *               UNIX_LINES = 0x01
    *
    *               CASE_INSENSITIVE = 0x02
    *
    *               COMMENTS = 0x04
    *
    *               MULTILINE = 0x08
    *
    *               LITERAL = 0x10
    *
    *               DOTALL = 0x20
    *
    *               UNICODE_CASE = 0x40
    *
    *               CANON_EQ = 0x80
    *
    *               UNICODE_CHARACTER_CLASS = 0x100
    */
  @cask.postJson("/match-regex-detailed")
  def matchRegexDetailed(regex: String, flags: Int): Response[Obj] = {
    try {
      val matchResults = PatternMatcher.matchRegexAgainstDatabase(regex, flags)
      val responseObj  = Obj("matchResults" -> Arr.from[PatternMatchResult](matchResults)(writeJs))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  /** @param regex see [[matchRegexDetailed]]
    * @param flags  see [[matchRegexDetailed]]
    */
  @cask.postJson("/match-regex")
  def matchRegex(regex: String, flags: Int): Response[Obj] = {
    try {
      val matchResults = PatternMatcher.matchRegexAgainstDatabase(regex, flags)
      val matchIds     = matchResults.map(_.programEntryId)
      val responseObj  = Obj("matchIds" -> Arr.from[Long](matchIds))
      cask.Response(data = responseObj, statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = Obj("errMsg" -> "Error occurred during retrieval"), statusCode = 500)
    }
  }

  @cask.postJson("/submit-program")
  def submitProgram(
    program: String,
    frontend: String,
    args: Array[String],
    originalVerifier: String,
    success: Boolean,
    runtime: Long
  ): Response[String] = {
    try {
      val userSubmission = UserSubmission(
        0,
        Timestamp.valueOf(LocalDateTime.now()),
        program,
        getLOC(program),
        frontend,
        args,
        originalVerifier,
        success,
        runtime
      )
      val insert = DBQueryInterface.insertUserSubmission(userSubmission)
      Await.ready(insert, DEFAULT_DB_TIMEOUT)

      cask.Response("Program submitted!", statusCode = 200)
    } catch {
      case e: Exception =>
        e.printStackTrace(); cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  initialize()
}
