package webAPI

import database.UserSubmission
import database.DBQueryInterface
import util.getLOC

import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Routes extends cask.MainRoutes {

  @cask.get("/")
  def default() = {
    "This is an API for the viper-data-collection database"
  }

  @cask.postJson("/submit-program")
  def submitProgram(originalName: String, program: String, frontend: String, args: Array[String], originalVerifier: String, success: Boolean, runtime: Long): Unit = {
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
    Await.ready(insert, Duration.Inf)
  }

  initialize()
}
