package database

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object ExecContext {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(8))
}

object DBQueryInterface {
  private val db = DBConnection.db
  import ExecContext._
  import GenericSlickTables.profile.api._

  def getAllProgramEntries(): Future[Seq[ProgramEntry]] = {
    val progBlobs: Future[Seq[ProgramEntryBlob]] = db.run(GenericSlickTables.programEntryTable.result)
    val programEntries: Future[Seq[ProgramEntry]] = progBlobs.map(s => s map (p => ProgramEntry.deBlob(p)))
    programEntries
  }

  def getAllUserSubmissions(): Future[Seq[UserSubmission]] = {
    val subBlobs: Future[Seq[UserSubmissionBlob]] = db.run(GenericSlickTables.userSubmissionTable.result)
    val userSubmissions: Future[Seq[UserSubmission]] = subBlobs.map(s => s map (u => UserSubmission.deBlob(u)))
    userSubmissions
  }

  def insertProgramEntry(entry: ProgramEntry): Future[Int] = {
    val blob = ProgramEntry.toBlob(entry)
    val insertQuery = GenericSlickTables.programEntryTable += blob
    db.run(insertQuery)
  }

  def insertUserSubmission(submission: UserSubmission): Future[Int] = {
    val blob = UserSubmission.toBlob(submission)
    val insertQuery = GenericSlickTables.userSubmissionTable += blob
    db.run(insertQuery)
  }

  def insertSiliconResult(result: SiliconResult): Future[Int] = {
    val blob = SiliconResult.toBlob(result)
    val insertQuery = GenericSlickTables.siliconResultTable += blob
    db.run(insertQuery)
  }

  def insertCarbonResult(result: CarbonResult): Future[Int] = {
    val blob = CarbonResult.toBlob(result)
    val insertQuery = GenericSlickTables.carbonResultTable += blob
    db.run(insertQuery)
  }

  def getOldestUserSubmission(): Future[Option[UserSubmission]] = {
    val subBlob: Future[Option[UserSubmissionBlob]] = db.run(GenericSlickTables.userSubmissionTable.sortBy(_.submissionDate.asc).result.headOption)
    val submission: Future[Option[UserSubmission]] = subBlob.map(s => s map (u => UserSubmission.deBlob(u)))
    submission
  }

}
