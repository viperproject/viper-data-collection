package dataCollection

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object ExecContext {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(8))
}

object DBQueryInterface {
  private val db = DBConnection.db
  import GenericSlickTables.profile.api._
  import ExecContext._

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

  def insertUserSubmission(submission: UserSubmission): Unit = {
    val blob = UserSubmission.toBlob(submission)
    val insertQuery = GenericSlickTables.userSubmissionTable += blob
    db.run(insertQuery)
  }

}
