package database

import dataCollection.EntryTuple
import slick.basic.DatabasePublisher

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

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

  def getPotentialMatchingEntryTuples(pe: ProgramEntry): DatabasePublisher[EntryTuple] = {
    val tupleQuery = for {
      proge <- PGSlickTables.programEntryTable
      sr <- PGSlickTables.siliconResultTable if sr.programEntryId === proge.programEntryId
      cr <- PGSlickTables.carbonResultTable if cr.programEntryId === proge.programEntryId
      pp <- PGSlickTables.programPrintEntryTable if pp.programEntryId === proge.programEntryId
    } yield (proge, pp, sr, cr)
    val filteredQuery = tupleQuery
      .filter(_._1.loc >= (pe.loc * 0.5).toInt)
      .filter(_._1.loc <= (pe.loc * 2.0).toInt)
      .filter(_._1.originalVerifier === pe.originalVerifier)
      .filter(_._1.frontend === pe.frontend)
      .filter(_._1.parseSuccess === pe.parseSuccess)
      .result
    val queryWithParams = filteredQuery.transactionally
      .withStatementParameters(fetchSize = 100)
    val tuples: DatabasePublisher[(ProgramEntry, ProgramPrintEntry, SiliconResult, CarbonResult)] = db.stream(queryWithParams)
    val entryTuples: DatabasePublisher[EntryTuple] = tuples.mapResult(t => EntryTuple.tupled(t))
    entryTuples
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

  def insertProgramEntry(entry: ProgramEntry): Future[Long] = {
    val insertQuery = (PGSlickTables.programEntryTable returning PGSlickTables.programEntryTable.map(_.programEntryId)) += entry
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

  def insertProgramPrintEntry(ppe: ProgramPrintEntry): Future[Int] = {
    val insertQuery = PGSlickTables.programPrintEntryTable += ppe
    db.run(insertQuery)
  }

  def insertEntry(et: EntryTuple): Future[Unit] = {
    val peId = Await.result(insertProgramEntry(et.programEntry), Duration.Inf)
    val inserts = DBIO.seq(
      PGSlickTables.siliconResultTable += et.siliconResult.copy(programEntryId = peId),
      PGSlickTables.carbonResultTable += et.carbonResult.copy(programEntryId = peId),
      PGSlickTables.programPrintEntryTable += et.programPrintEntry.copy(programEntryId = peId)
    )
    Await.ready(db.run(inserts), Duration.Inf)
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

  def getUSCount(): Future[Int] = {
    val usCount = db.run(PGSlickTables.userSubmissionTable.length.result)
    usCount
  }

  def getPECount(): Future[Int] = {
    val peCount = db.run(PGSlickTables.programEntryTable.length.result)
    peCount
  }

  def getSRCount(): Future[Int] = {
    val srCount = db.run(PGSlickTables.siliconResultTable.length.result)
    srCount
  }

  def getUniqueSRCount(): Future[Int] = {
    val srCount = db.run(PGSlickTables.siliconResultTable.groupBy(_.programEntryId).length.result)
    srCount
  }

  def getUniqueCRCount(): Future[Int] = {
    val crCount = db.run(PGSlickTables.carbonResultTable.groupBy(_.programEntryId).length.result)
    crCount
  }

  def getCRCount(): Future[Int] = {
    val crCount = db.run(PGSlickTables.carbonResultTable.length.result)
    crCount
  }

  def getPPCount(): Future[Int] = {
    val ppCount = db.run(PGSlickTables.programPrintEntryTable.length.result)
    ppCount
  }


  def getFrontendCount(frontend: String): Future[Int] = {
    val feCount = db.run(PGSlickTables.programEntryTable.filter(_.frontend === frontend).length.result)
    feCount
  }

  def getVerifierCount(verifier: String): Future[Int] = {
    val vCount = db.run(PGSlickTables.programEntryTable.filter(_.originalVerifier === verifier).length.result)
    vCount
  }

  def getParseSuccessCount(success: Boolean): Future[Int] = {
    val psCount = db.run(PGSlickTables.programEntryTable.filter(_.parseSuccess === success).length.result)
    psCount
  }

  def getSilSuccessCount(success: Boolean): Future[Int] = {
    val ssCount = db.run(PGSlickTables.siliconResultTable.filter(_.success === success).length.result)
    ssCount
  }

  def getCarbonSuccessCount(success: Boolean): Future[Int] = {
    val csCount = db.run(PGSlickTables.carbonResultTable.filter(_.success === success).length.result)
    csCount
  }

  def getLocRangeCount(lower: Int, upper: Int): Future[Int] = {
    val lrCount = db.run(PGSlickTables.programEntryTable.filter(_.loc >= lower).filter(_.loc <= upper).length.result)
    lrCount
  }

  def getSiliconRuntimeRangeCount(lower: Long, upper: Long): Future[Int] = {
    val srrCount = db.run(PGSlickTables.siliconResultTable.filter(_.runtime >= lower).filter(_.runtime <= upper).length.result)
    srrCount
  }

  def getCarbonRuntimeRangeCount(lower: Long, upper: Long): Future[Int] = {
    val crrCount = db.run(PGSlickTables.carbonResultTable.filter(_.runtime >= lower).filter(_.runtime <= upper).length.result)
    crrCount
  }

  def clearDB(): Future[Unit] = {
    val deleteUS = PGSlickTables.userSubmissionTable.delete
    val deletePE = PGSlickTables.programEntryTable.delete
    val deleteSR = PGSlickTables.siliconResultTable.delete
    val deleteCR = PGSlickTables.carbonResultTable.delete
    val deletePP = PGSlickTables.programPrintEntryTable.delete
    val deleteQuery = DBIO.seq(deleteUS, deletePE, deleteSR, deleteCR, deletePP)
    db.run(deleteQuery)
  }

}
