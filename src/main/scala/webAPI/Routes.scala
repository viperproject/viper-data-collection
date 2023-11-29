package webAPI

import database.DBQueryInterface
import JSONReadWriters._
import queryFrontend.UserSubmission
import queryFrontend.JSONReadWriters._
import cask.Response
import database.tools.PatternMatcher
import util._
import util.Config._
import upickle.default._

import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await

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

  @cask.get("/")
  def default() = {
    "This is an API for the viper-data-collection database"
  }

  @cask.postJson("/program-entries-by-meta-data")
  def programEntriesByMetadata(
    earliestDate: Timestamp,
    latestDate: Timestamp,
    minLOC: Int,
    maxLOC: Int,
    frontend: Option[String],
    verifier: Option[String],
    parseSuccess: Option[Boolean]
  ): Response[String] = {
    try {
      val entries = Await.result(
        DBQueryInterface
          .getEntriesByFeatures(earliestDate, latestDate, minLOC, maxLOC, frontend, verifier, parseSuccess),
        DEFAULT_DB_TIMEOUT
      )
      val eJSON = write(entries)
      cask.Response(data = eJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.get("/program-ids-by-feature-value")
  def programIdsByFeatureValue(feature: String, value: String): Response[String] = {
    try {
      val peIds  = Await.result(DBQueryInterface.getPEIdsWithFeatureValue(feature, value), DEFAULT_DB_TIMEOUT)
      val idJSON = write(peIds)
      cask.Response(data = idJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/silicon-results-by-ids")
  def siliconResultsByIds(entryIds: Seq[Long]): Response[String] = {
    try {
      val results = Await.result(DBQueryInterface.getSiliconResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val rJSON   = write(results)
      cask.Response(data = rJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/carbon-results-by-ids")
  def carbonResultsByIds(entryIds: Seq[Long]): Response[String] = {
    try {
      val results = Await.result(DBQueryInterface.getCarbonResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val rJSON   = write(results)
      cask.Response(data = rJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/frontend-count-by-ids")
  def frontendCountByIds(entryIds: Seq[Long]): Response[String] = {
    try {
      val frontendCounts = Await.result(DBQueryInterface.getFrontendCountByIds(entryIds), DEFAULT_DB_TIMEOUT)
      val fcJSON         = write(frontendCounts)
      cask.Response(data = fcJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.get("/match-regex-detailed")
  def matchRegexDetailed(regex: String, flags: Int): Response[String] = {
    try {
      val matchResults = PatternMatcher.matchRegexAgainstDatabase(regex, flags)
      val mrJSON       = write(matchResults)
      cask.Response(data = mrJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/match-regex")
  def matchRegex(regex: String, flags: Int): Response[String] = {
    try {
      val matchResults = PatternMatcher.matchRegexAgainstDatabase(regex, flags)
      val matchIds     = matchResults.map(_.programEntryId)
      val miJSON       = write(matchIds)
      cask.Response(data = miJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/submit-program")
  def submitProgram(
    originalName: String,
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
        originalName,
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
      case _ => cask.Response(data = "Error occurred during submission", statusCode = 500)
    }
  }

  initialize()
}
