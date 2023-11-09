package database

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object ExecContext {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(8))
}

object DBQueryInterface {
  private val db = DBConnection.db

  import ExecContext._
  import PGSlickTables.profile.api._

  def getAllProgramEntries(): Future[Seq[ProgramEntry]] = {
    val programEntries: Future[Seq[ProgramEntry]] = db.run(PGSlickTables.programEntryTable.result)
    programEntries
  }

  def getProgramEntryByID(programEntryId: Long): Future[Option[ProgramEntry]] = {
    val entryOpt = db.run(PGSlickTables.programEntryTable.filter(_.programEntryId === programEntryId).result.headOption)
    entryOpt
  }

  def getPotentialMatchingEntries(pe: ProgramEntry): Future[Seq[ProgramEntry]] = {
    val query = PGSlickTables.programEntryTable
      .filter(_.loc >= (pe.loc * 0.8).toInt)
      .filter(_.loc <= (pe.loc * 1.2).toInt)
      .filter(_.originalVerifier === pe.originalVerifier)
      .filter(_.frontend === pe.frontend)
      .filter(_.parseSuccess === pe.parseSuccess)
      .result
    val programEntries: Future[Seq[ProgramEntry]] = db.run(query)
    programEntries
  }

  def getPEIdsWithoutSilRes(): Future[Seq[Long]] = {
    val query = for {
      entry <- PGSlickTables.programEntryTable if !PGSlickTables.siliconResultTable.filter(_.programEntryId === entry.programEntryId).exists
    } yield entry.programEntryId
    val ids = db.run(query.result)
    ids
  }

  def getPEIdsWithoutCarbRes(): Future[Seq[Long]] = {
    val query = for {
      entry <- PGSlickTables.programEntryTable if !PGSlickTables.carbonResultTable.filter(_.programEntryId === entry.programEntryId).exists
    } yield entry.programEntryId
    val ids = db.run(query.result)
    ids
  }

  def getAllUserSubmissions(): Future[Seq[UserSubmission]] = {
    val userSubmissions: Future[Seq[UserSubmission]] = db.run(PGSlickTables.userSubmissionTable.result)
    userSubmissions
  }

  def insertProgramEntry(entry: ProgramEntry): Future[Int] = {
    val insertQuery = PGSlickTables.programEntryTable += entry
    db.run(insertQuery)
  }

  def insertUserSubmission(submission: UserSubmission): Future[Int] = {
    val insertQuery = PGSlickTables.userSubmissionTable += submission
    db.run(insertQuery)
  }

  def deleteUserSubmission(submissionId: Long): Future[Int] = {
    val query = PGSlickTables.userSubmissionTable.filter(_.submissionId === submissionId).delete
    db.run(query)
  }
  def insertSiliconResult(result: SiliconResult): Future[Int] = {
    val insertQuery = PGSlickTables.siliconResultTable += result
    db.run(insertQuery)
  }

  def insertCarbonResult(result: CarbonResult): Future[Int] = {
    val insertQuery = PGSlickTables.carbonResultTable += result
    db.run(insertQuery)
  }

  def getSiliconResultsForEntry(peId: Long): Future[Seq[SiliconResult]] = {
    val siliconResults = db.run(PGSlickTables.siliconResultTable.filter(_.programEntryId === peId).result)
    siliconResults
  }

  def getLatestSilResForEntry(peId: Long): Future[Option[SiliconResult]] = {
    val silResOpt = db.run(PGSlickTables.siliconResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    silResOpt
  }

  def getLatestCarbResForEntry(peId: Long): Future[Option[CarbonResult]] = {
    val carbResOpt = db.run(PGSlickTables.carbonResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    carbResOpt
  }

  def getCarbonResultsForEntry(peId: Long): Future[Seq[CarbonResult]] = {
    val carbonResults = db.run(PGSlickTables.carbonResultTable.filter(_.programEntryId === peId).result)
    carbonResults
  }

  def getOldestUserSubmission(): Future[Option[UserSubmission]] = {
    val submission: Future[Option[UserSubmission]] = db.run(PGSlickTables.userSubmissionTable.sortBy(_.submissionDate.asc).result.headOption)
    submission
  }

}
