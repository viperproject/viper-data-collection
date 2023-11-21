package webAPI

import database.UserSubmission
import database.DBQueryInterface
import JSONReadWriters._
import cask.Response
import dataCollection.PatternMatcher
import util._
import util.Config._
import upickle.default._

import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object Routes extends cask.MainRoutes {


  @cask.get("/")
  def default() = {
    "This is an API for the viper-data-collection database"
  }

  @cask.postJson("/program-entry-by-feature")
  def programEntryByFeature(earliestDate: Timestamp,
                            latestDate: Timestamp,
                            minLOC: Int,
                            maxLOC: Int,
                            frontend: Option[String],
                            verifier: Option[String],
                            parseSuccess: Option[Boolean]): Response[String] = {
    try {
      val entries = Await.result(DBQueryInterface.getEntriesByFeatures(earliestDate, latestDate, minLOC, maxLOC, frontend, verifier, parseSuccess), DEFAULT_DB_TIMEOUT)
      val eJSON = write(entries)
      cask.Response(data = eJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/silicon-results-by-ids")
  def siliconResultsByIds(entryIds: Seq[Long]): Response[String] = {
    try {
      val results = Await.result(DBQueryInterface.getSiliconResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val rJSON = write(results)
      cask.Response(data = rJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/carbon-results-by-ids")
  def carbonResultsByIds(entryIds: Seq[Long]): Response[String] = {
    try {
      val results = Await.result(DBQueryInterface.getCarbonResultsForEntries(entryIds), DEFAULT_DB_TIMEOUT)
      val rJSON = write(results)
      cask.Response(data = rJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/match-regex")
  def matchRegex(regex: String): Response[String] = {
    try {
      val matchResults = PatternMatcher.matchRegexAgainstDatabase(regex)
      val mrJSON = write(matchResults)
      cask.Response(data = mrJSON, statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during retrieval", statusCode = 500)
    }
  }

  @cask.postJson("/submit-program")
  def submitProgram(originalName: String, program: String, frontend: String, args: Array[String], originalVerifier: String, success: Boolean, runtime: Long): Response[String] = {
    try {
      val userSubmission = UserSubmission(0,
        Timestamp.valueOf(LocalDateTime.now()),
        originalName,
        program,
        getLOC(program),
        frontend,
        args,
        originalVerifier,
        success,
        runtime)
      val insert = DBQueryInterface.insertUserSubmission(userSubmission)
      Await.ready(insert, DEFAULT_DB_TIMEOUT)

      cask.Response("Program submitted!", statusCode = 200)
    } catch {
      case _ => cask.Response(data = "Error occurred during submission", statusCode = 500)
    }
  }

  initialize()
}
