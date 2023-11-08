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
    val progBlobs: Future[Seq[ProgramEntryBlob]] = db.run(PGSlickTables.programEntryTable.result)
    val programEntries: Future[Seq[ProgramEntry]] = deBlobPE(progBlobs)
    programEntries
  }

  def getPotentialMatchingEntries(pe: ProgramEntry): Future[Seq[ProgramEntry]] = {
    val query = PGSlickTables.programEntryTable
      .filter(_.loc >= (pe.loc * 0.8).toInt)
      .filter(_.loc <= (pe.loc * 1.2).toInt)
      .filter(_.originalVerifier === pe.originalVerifier)
      .filter(_.frontend === pe.frontend)
      .filter(_.parseSuccess === pe.parseSuccess)
      .result
    val progBlobs: Future[Seq[ProgramEntryBlob]] = db.run(query)
    val programEntries = deBlobPE(progBlobs)
    programEntries
  }

  def getAllUserSubmissions(): Future[Seq[UserSubmission]] = {
    val subBlobs: Future[Seq[UserSubmissionBlob]] = db.run(PGSlickTables.userSubmissionTable.result)
    val userSubmissions: Future[Seq[UserSubmission]] = deBlobUS(subBlobs)
    userSubmissions
  }

  def insertProgramEntry(entry: ProgramEntry): Future[Int] = {
    val blob = ProgramEntry.toBlob(entry)
    val insertQuery = PGSlickTables.programEntryTable += blob
    db.run(insertQuery)
  }

  def insertUserSubmission(submission: UserSubmission): Future[Int] = {
    val blob = UserSubmission.toBlob(submission)
    val insertQuery = PGSlickTables.userSubmissionTable += blob
    db.run(insertQuery)
  }

  def insertSiliconResult(result: SiliconResult): Future[Int] = {
    val blob = SiliconResult.toBlob(result)
    val insertQuery = PGSlickTables.siliconResultTable += blob
    db.run(insertQuery)
  }

  def insertCarbonResult(result: CarbonResult): Future[Int] = {
    val blob = CarbonResult.toBlob(result)
    val insertQuery = PGSlickTables.carbonResultTable += blob
    db.run(insertQuery)
  }

  def getSiliconResultsForEntry(peId: Long): Future[Seq[SiliconResult]] = {
    val silResBlobs = db.run(PGSlickTables.siliconResultTable.filter(_.programEntryId === peId).result)
    val siliconResults = deblobSR(silResBlobs)
    siliconResults
  }

  def getLatestSilResForEntry(peId: Long): Future[Option[SiliconResult]] = {
    val silResBlobOpt = db.run(PGSlickTables.siliconResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    val silResOpt = silResBlobOpt map (o => o map (s => SiliconResult.deBlob(s)))
    silResOpt
  }

  def getLatestCarbResForEntry(peId: Long): Future[Option[CarbonResult]] = {
    val carbResBlobOpt = db.run(PGSlickTables.carbonResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    val carbResOpt = carbResBlobOpt map (o => o map (c => CarbonResult.deBlob(c)))
    carbResOpt
  }

  def getCarbonResultsForEntry(peId: Long): Future[Seq[CarbonResult]] = {
    val carbResBlobs = db.run(PGSlickTables.carbonResultTable.filter(_.programEntryId === peId).result)
    val carbonResults = deblobCR(carbResBlobs)
    carbonResults
  }

  def getOldestUserSubmission(): Future[Option[UserSubmission]] = {
    val subBlob: Future[Option[UserSubmissionBlob]] = db.run(PGSlickTables.userSubmissionTable.sortBy(_.submissionDate.asc).result.headOption)
    val submission: Future[Option[UserSubmission]] = subBlob.map(s => s map (u => UserSubmission.deBlob(u)))
    submission
  }

  private def deBlobPE(list: Future[Seq[ProgramEntryBlob]]): Future[Seq[ProgramEntry]] = {
    list.map(s => s map (p => ProgramEntry.deBlob(p)))
  }

  private def deBlobUS(list: Future[Seq[UserSubmissionBlob]]): Future[Seq[UserSubmission]] = {
    list.map(s => s map (u => UserSubmission.deBlob(u)))
  }

  private def deblobSR(list: Future[Seq[SiliconResultBlob]]): Future[Seq[SiliconResult]] = {
    list.map(s => s map (r => SiliconResult.deBlob(r)))
  }

  private def deblobCR(list: Future[Seq[CarbonResultBlob]]): Future[Seq[CarbonResult]] = {
    list.map(s => s map (r => CarbonResult.deBlob(r)))
  }

}
