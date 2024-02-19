package database

import dataCollection.{ProcessingResultTuple, ProgramPrint, ProgramTuple}
import dataCollection.customFrontends.VerifierFeature
import slick.basic.DatabasePublisher
import util.Config._
import queryFrontend._

import java.sql.Timestamp
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}

/** ExecutionContext in which queries are run */
object DBExecContext {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newWorkStealingPool(DB_EXEC_CONTEXT_PARALLELISM))
}

/** Contains slick queries to fetch entries from the database */
object DBQueryInterface {
  private val db      = DBConnection.db
  val sTables = PGSlickTables

  import sTables.profile.api._
  import DBExecContext._

  /*------ ProgramEntry Queries ------*/
  def getAllProgramEntries(): Future[Seq[ProgramEntry]] = {
    val programEntries: Future[Seq[ProgramEntry]] = db.run(sTables.programEntryTable.result)
    programEntries
  }

  def getProgramEntriesStream(): DatabasePublisher[ProgramEntry] = {
    val query = sTables.programEntryTable.result.transactionally.withStatementParameters(fetchSize = DB_BATCH_SIZE)
    val publisher: DatabasePublisher[ProgramEntry] = db.stream(query)
    publisher
  }

  def getAllProgramEntriesBatched(): DatabasePublisher[ProgramEntry] = {
    val query       = sTables.programEntryTable.result.transactionally.withStatementParameters(fetchSize = DB_BATCH_SIZE)
    val entryStream = db.stream(query)
    entryStream
  }

  def getProgramEntryByID(programEntryId: Long): Future[Option[ProgramEntry]] = {
    val entryOpt = db.run(sTables.programEntryTable.filter(_.programEntryId === programEntryId).result.headOption)
    entryOpt
  }

  def getProgramEntriesByIDs(entryIds: Seq[Long]): Future[Seq[ProgramEntry]] = {
    val uniqueIds = entryIds.toSet
    val entrySeq  = db.run(sTables.programEntryTable.filter(_.programEntryId.inSet(uniqueIds)).result)
    entrySeq
  }

  def getEntriesByMetadata(
    earliestDate: Timestamp,
    latestDate: Timestamp,
    minLOC: Int,
    maxLOC: Int,
    frontend: Option[String],
    verifier: Option[String],
    parseSuccess: Option[Boolean]
  ): Future[Seq[ProgramEntry]] = {
    val baseQuery = sTables.programEntryTable
    val rangeFilter = baseQuery
      .filter(_.submissionDate >= earliestDate)
      .filter(_.submissionDate <= latestDate)
      .filter(_.loc >= minLOC)
      .filter(_.loc <= maxLOC)
    val frontendFilter = frontend match {
      case Some(f) => rangeFilter.filter(_.frontend === f)
      case None    => rangeFilter
    }
    val verifierFilter = verifier match {
      case Some(v) => frontendFilter.filter(_.originalVerifier === v)
      case None    => frontendFilter
    }
    val parseFilter = parseSuccess match {
      case Some(p) => verifierFilter.filter(_.parseSuccess === p)
      case None    => verifierFilter
    }
    val entries: Future[Seq[ProgramEntry]] = db.run(parseFilter.result)
    entries
  }

  def insertProgramEntries(entries: Seq[ProgramEntry]) = {
    val insertQuery = sTables.programEntryTable ++= entries
    db.run(insertQuery)
  }

  def insertProgramEntry(entry: ProgramEntry): Future[Long] = {
    val insertQuery = (sTables.programEntryTable returning sTables.programEntryTable.map(_.programEntryId)) += entry
    db.run(insertQuery)
  }

  /*------ ProgramEntryId Queries ------*/

  def getPEIdsWithFeatureValue(feature: String, value: String): Future[Seq[Long]] = {
    val silFeatQuery = (for {
      feat <- sTables.silFeatureEntryTable if feat.featureName === feature && feat.value === value
      silRes <- sTables.siliconResultTable if feat.resultId === silRes.silResId
    } yield silRes.programEntryId).result
    val carbFeatQuery = (for {
      feat <- sTables.carbFeatureEntryTable if feat.featureName === feature && feat.value === value
      carbRes <- sTables.carbonResultTable if feat.resultId === carbRes.carbResId
    } yield carbRes.programEntryId).result
    val constFeatQuery = (for {
      feat <- sTables.constFeatureEntryTable if feat.featureName === feature && feat.value === value
    } yield feat.programEntryId).result
    db.run(DBIO.sequence(Seq(silFeatQuery, carbFeatQuery, constFeatQuery))).map(_.flatten)
  }

  def getPEIdsWithoutSilVersionRes(versionHash: String): Future[Seq[Long]] = {
    val query = for {
      entry <- sTables.programEntryTable if !sTables.siliconResultTable
        .filter(sr =>
          sr.programEntryId === entry.programEntryId
          && sr.verifierHash === versionHash
        )
        .exists
    } yield entry.programEntryId
    val ids = db.run(query.result)
    ids
  }

  def getPEIdsWithoutCarbVersionRes(versionHash: String): Future[Seq[Long]] = {
    val query = for {
      entry <- sTables.programEntryTable if !sTables.carbonResultTable
        .filter(cr =>
          cr.programEntryId === entry.programEntryId
          && cr.verifierHash === versionHash
        )
        .exists
    } yield entry.programEntryId
    val ids = db.run(query.result)
    ids
  }

  def getPEIdsBySiliconData(
    success: Boolean,
    didTimeout: Boolean,
    minRuntime: Long,
    maxRuntime: Long
  ): Future[Seq[Long]] = {
    val silResQuery   = sTables.siliconResultTable
    val successFilter = silResQuery.filter(_.success === success)
    val timeoutFilter = successFilter.filter(_.didTimeout === didTimeout)
    val runtimeFilter = timeoutFilter.filter(s => s.runtime >= minRuntime && s.runtime <= maxRuntime)
    val ids           = runtimeFilter.groupBy(_.programEntryId).map(_._1).result
    db.run(ids)
  }

  def getPEIdsByCarbonData(
    success: Boolean,
    didTimeout: Boolean,
    minRuntime: Long,
    maxRuntime: Long
  ): Future[Seq[Long]] = {
    val carbResQuery  = sTables.carbonResultTable
    val successFilter = carbResQuery.filter(_.success === success)
    val timeoutFilter = successFilter.filter(_.didTimeout === didTimeout)
    val runtimeFilter = timeoutFilter.filter(s => s.runtime >= minRuntime && s.runtime <= maxRuntime)
    val ids           = runtimeFilter.groupBy(_.programEntryId).map(_._1).result
    db.run(ids)
  }

  /*------ UserSubmission Queries ------*/

  def getAllUserSubmissions(): Future[Seq[UserSubmission]] = {
    val userSubmissions: Future[Seq[UserSubmission]] = db.run(sTables.userSubmissionTable.result)
    userSubmissions
  }

  def insertUserSubmission(submission: UserSubmission): Future[Int] = {
    val insertQuery = sTables.userSubmissionTable += submission
    db.run(insertQuery)
  }

  def getOldestUserSubmission(): Future[Option[UserSubmission]] = {
    val submission: Future[Option[UserSubmission]] =
      db.run(sTables.userSubmissionTable.sortBy(_.submissionDate.asc).result.headOption)
    submission
  }

  def deleteUserSubmission(submissionId: Long): Future[Int] = {
    val query = sTables.userSubmissionTable.filter(_.submissionId === submissionId).delete
    db.run(query)
  }

  /*------ VerResult Queries ------*/

  def insertSiliconResult(result: VerResult): Future[Long] = {
    val insertQuery = (sTables.siliconResultTable returning sTables.siliconResultTable.map(_.silResId)) += result
    db.run(insertQuery)
  }

  def insertCarbonResult(result: VerResult): Future[Long] = {
    val insertQuery = (sTables.carbonResultTable returning sTables.carbonResultTable.map(_.carbResId)) += result
    db.run(insertQuery)
  }

  def getSiliconResultsForEntry(peId: Long): Future[Seq[VerResult]] = {
    val siliconResults = db.run(sTables.siliconResultTable.filter(_.programEntryId === peId).result)
    siliconResults
  }

  def getSiliconResultsForEntries(peIds: Seq[Long]): Future[Seq[VerResult]] = {
    val idSet          = peIds.toSet
    val siliconResults = db.run(sTables.siliconResultTable.filter(s => s.programEntryId.inSet(idSet)).result)
    siliconResults
  }

  def getLatestSilResForEntry(peId: Long): Future[Option[VerResult]] = {
    val silResOpt =
      db.run(sTables.siliconResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    silResOpt
  }

  def getLatestCarbResForEntry(peId: Long): Future[Option[VerResult]] = {
    val carbResOpt =
      db.run(sTables.carbonResultTable.filter(_.programEntryId === peId).sortBy(_.creationDate.desc).result.headOption)
    carbResOpt
  }

  def getCarbonResultsForEntry(peId: Long): Future[Seq[VerResult]] = {
    val carbonResults = db.run(sTables.carbonResultTable.filter(_.programEntryId === peId).result)
    carbonResults
  }

  def getCarbonResultsForEntries(peIds: Seq[Long]): Future[Seq[VerResult]] = {
    val idSet         = peIds.toSet
    val carbonResults = db.run(sTables.carbonResultTable.filter(c => c.programEntryId.inSet(idSet)).result)
    carbonResults
  }

  def getSilResForVersion(versionHash: String): Future[Seq[VerResult]] = {
    val silRes = db.run(sTables.siliconResultTable.filter(_.verifierHash === versionHash).result)
    silRes
  }

  def getCarbResForVersion(versionHash: String): Future[Seq[VerResult]] = {
    val carbRes = db.run(sTables.carbonResultTable.filter(_.verifierHash === versionHash).result)
    carbRes
  }

  /*------ ProgramPrintEntry Queries ------*/

  def insertProgramPrintEntry(ppe: ProgramPrintEntry): Future[Int] = {
    val insertQuery = sTables.programPrintEntryTable += ppe
    db.run(insertQuery)
  }

  def insertProgramPrintEntries(ppe: Seq[ProgramPrintEntry]): Future[Any] = {
    val insertQuery = sTables.programPrintEntryTable ++= ppe
    db.run(insertQuery)
  }

  def updateProgramPrintEntry(ppe: ProgramPrintEntry) = {
    val q = for {entry <- sTables.programPrintEntryTable if entry.programEntryId === ppe.programEntryId} yield entry.pprintId
    val pKey = Await.result(db.run(q.result.headOption), DEFAULT_DB_TIMEOUT)
    val newPPE = ppe.copy(pprintId = pKey.getOrElse(0))
    sTables.programPrintEntryTable.insertOrUpdate(newPPE)
  }

  def getAllProgramPrintEntries(): Future[Seq[ProgramPrintEntry]] = {
    db.run(sTables.programPrintEntryTable.result)
  }

  /*------ Metadata Queries ------*/

  def getUSCount(): Future[Int] = {
    val usCount = db.run(sTables.userSubmissionTable.length.result)
    usCount
  }

  def getPECount(): Future[Int] = {
    val peCount = db.run(sTables.programEntryTable.length.result)
    peCount
  }

  def getSRCount(): Future[Int] = {
    val srCount = db.run(sTables.siliconResultTable.length.result)
    srCount
  }

  def getUniqueSRCount(): Future[Int] = {
    val srCount = db.run(sTables.siliconResultTable.groupBy(_.programEntryId).length.result)
    srCount
  }

  def getUniqueCRCount(): Future[Int] = {
    val crCount = db.run(sTables.carbonResultTable.groupBy(_.programEntryId).length.result)
    crCount
  }

  def getCRCount(): Future[Int] = {
    val crCount = db.run(sTables.carbonResultTable.length.result)
    crCount
  }

  def getPPCount(): Future[Int] = {
    val ppCount = db.run(sTables.programPrintEntryTable.length.result)
    ppCount
  }

  def getFrontendCount(frontend: String): Future[Int] = {
    val feCount = db.run(sTables.programEntryTable.filter(_.frontend === frontend).length.result)
    feCount
  }

  def getFrontendCountByIds(peIds: Seq[Long]): Future[Seq[(String, Int)]] = {
    val idSet = peIds.toSet
    val query = sTables.programEntryTable
      .filter(_.programEntryId.inSet(idSet))
      .groupBy(_.frontend)
      .map { case (f, results) =>
        (f, results.length)
      }
      .result
    db.run(query)
  }

  def getVerifierCount(verifier: String): Future[Int] = {
    val vCount = db.run(sTables.programEntryTable.filter(_.originalVerifier === verifier).length.result)
    vCount
  }

  def getParseSuccessCount(success: Boolean): Future[Int] = {
    val psCount = db.run(sTables.programEntryTable.filter(_.parseSuccess === success).length.result)
    psCount
  }

  def getSilSuccessCount(success: Boolean): Future[Int] = {
    val ssCount = db.run(sTables.siliconResultTable.filter(_.success === success).length.result)
    ssCount
  }

  def getCarbonSuccessCount(success: Boolean): Future[Int] = {
    val csCount = db.run(sTables.carbonResultTable.filter(_.success === success).length.result)
    csCount
  }

  def getLocRangeCount(lower: Int, upper: Int): Future[Int] = {
    val lrCount = db.run(sTables.programEntryTable.filter(_.loc >= lower).filter(_.loc <= upper).length.result)
    lrCount
  }

  def getSiliconRuntimeRangeCount(lower: Long, upper: Long): Future[Int] = {
    val srrCount =
      db.run(sTables.siliconResultTable.filter(_.runtime >= lower).filter(_.runtime <= upper).length.result)
    srrCount
  }

  def getCarbonRuntimeRangeCount(lower: Long, upper: Long): Future[Int] = {
    val crrCount = db.run(sTables.carbonResultTable.filter(_.runtime >= lower).filter(_.runtime <= upper).length.result)
    crrCount
  }

  /*------ Feature Queries ------*/

  def insertIfNotExistsFeature(name: String): Future[Any] = {
    val query = (for (f <- sTables.featureTable if f.name === name) yield f).exists.result.flatMap { exists =>
      {
        if (!exists) sTables.featureTable += Feature(name)
        else DBIO.successful(None)
      }
    }
    db.run(query)
  }

  def insertIfNotExistsConstFeature(cf: VerifierFeature, programEntryId: Long) = {
    val query =
      (for (f <- sTables.constFeatureEntryTable if f.programEntryId === programEntryId && f.featureName === cf.name)
        yield f).exists.result.flatMap { exists =>
        if (!exists) sTables.constFeatureEntryTable += FeatureEntry(0, cf.name, programEntryId, cf.value)
        else DBIO.successful(None)
      }
    query
  }

  def insertVerifierFeatures(
    verifier: String,
    resultId: Long,
    programEntryId: Long,
    vfs: Seq[VerifierFeature]
  ): Future[Any] = {
    val partition = vfs.partition(_.constant)
    Await.ready(
      Future.sequence(vfs map (vf => insertIfNotExistsFeature(vf.name))),
      DEFAULT_DB_TIMEOUT
    )
    val verifierInserts = if (verifier == "Silicon") {
      sTables.silFeatureEntryTable ++= partition._2 map (vf => FeatureEntry(0, vf.name, resultId, vf.value))
    } else {
      sTables.carbFeatureEntryTable ++= partition._2 map (vf => FeatureEntry(0, vf.name, resultId, vf.value))
    }
    val constInserts = partition._1 map (cf => insertIfNotExistsConstFeature(cf, programEntryId))
    db.run(DBIO.seq(DBIO.sequence(constInserts :+ verifierInserts)))
  }

  def insertFeature(feature: Feature): Future[Int] = {
    val insertQuery = sTables.featureTable += feature
    db.run(insertQuery)
  }

  def getFeatCount(): Future[Int] = {
    val featCount = db.run(sTables.featureTable.length.result)
    featCount
  }

  def getFeatureCount(feature: String): Future[Int] = {
    val silCount = (for {
      f   <- sTables.featureTable if f.name === feature
      sfe <- sTables.silFeatureEntryTable if sfe.featureName === f.name
    } yield sfe).length.result
    val carbCount = (for {
      f   <- sTables.featureTable if f.name === feature
      sfe <- sTables.carbFeatureEntryTable if sfe.featureName === f.name
    } yield sfe).length.result
    val sCount = db.run(silCount)
    val cCount = db.run(carbCount)
    sCount flatMap (s => cCount map (c => s + c))
  }

  def getFeaturesById(id: Long): Future[Seq[FeatureEntry]] = {
    val silFeatQuery = (for {
      silRes <- sTables.siliconResultTable if silRes.programEntryId === id
      feat   <- sTables.silFeatureEntryTable.filter(_.resultId === silRes.silResId)
    } yield feat).result
    val carbFeatQuery = (for {
      carbRes <- sTables.carbonResultTable if carbRes.programEntryId === id
      feat    <- sTables.carbFeatureEntryTable.filter(_.resultId === carbRes.carbResId)
    } yield feat).result
    val constFeatQuery = (for {
      feat <- sTables.constFeatureEntryTable if feat.programEntryId === id
    } yield feat).result
    db.run(DBIO.sequence(Seq(silFeatQuery, carbFeatQuery, constFeatQuery))).map(_.flatten)
  }

  /*------ General Queries ------*/

  def getPotentialMatchingEntryTuples(pe: ProgramEntry): DatabasePublisher[ProgramTuple] = {
    val tupleQuery = for {
      proge <- sTables.programEntryTable
      sr    <- sTables.siliconResultTable if sr.programEntryId === proge.programEntryId
      cr    <- sTables.carbonResultTable if cr.programEntryId === proge.programEntryId
      pp    <- sTables.programPrintEntryTable if pp.programEntryId === proge.programEntryId
    } yield (proge, pp, sr, cr)
    val filteredQuery = tupleQuery
      .filter(_._1.loc >= (pe.loc * 0.5).toInt)
      .filter(_._1.loc <= (pe.loc * 2.0).toInt)
      .filter(_._1.originalVerifier === pe.originalVerifier)
      .filter(_._1.frontend === pe.frontend)
      .filter(_._1.parseSuccess === pe.parseSuccess)
      .result
    val queryWithParams = filteredQuery.transactionally
      .withStatementParameters(fetchSize = DB_BATCH_SIZE)
    val tuples: DatabasePublisher[(ProgramEntry, ProgramPrintEntry, VerResult, VerResult)] = db.stream(queryWithParams)
    val entryTuples: DatabasePublisher[ProgramTuple]                                       = tuples.mapResult(t => ProgramTuple.tupled(t))
    entryTuples
  }

  def getPotentialMatchingPrograms(pe: ProgramEntry): DatabasePublisher[(ProgramEntry, ProgramPrintEntry)] = {
    val tupleQuery = for {
      proge <- sTables.programEntryTable
      pp    <- sTables.programPrintEntryTable if pp.programEntryId === proge.programEntryId
    } yield (proge, pp)
    val filteredQuery = tupleQuery
      .filter(_._1.loc >= (pe.loc * 0.5).toInt)
      .filter(_._1.loc <= (pe.loc * 2.0).toInt)
      .filter(_._1.originalVerifier === pe.originalVerifier)
      .filter(_._1.frontend === pe.frontend)
      .filter(_._1.parseSuccess === pe.parseSuccess)
      .result
    val queryWithParams = filteredQuery.transactionally
      .withStatementParameters(fetchSize = DB_BATCH_SIZE)
    val tuples: DatabasePublisher[(ProgramEntry, ProgramPrintEntry)] = db.stream(queryWithParams)
    tuples
  }

  def insertProcessingResult(prt: ProcessingResultTuple): Future[Any] = {
    val resInsert = for {
      peId <- (sTables.programEntryTable returning sTables.programEntryTable.map(
        _.programEntryId
      )) += prt.programTuple.programEntry
      silResId <- (sTables.siliconResultTable returning sTables.siliconResultTable.map(
        _.silResId
      )) += prt.programTuple.siliconResult.copy(programEntryId = peId)
      carbResId <- (sTables.carbonResultTable returning sTables.carbonResultTable.map(
        _.carbResId
      )) += prt.programTuple.carbonResult.copy(programEntryId = peId)
    } yield (peId, silResId, carbResId)
    val (peId, silResId, carbResId) = Await.result(db.run(resInsert), DEFAULT_DB_TIMEOUT)
    val ppeInsert                   = insertProgramPrintEntry(prt.programTuple.programPrintEntry.copy(programEntryId = peId))
    val silFeatureInserts           = insertVerifierFeatures("Silicon", silResId, peId, prt.silVerFeatures)
    val carbFeatureInserts          = insertVerifierFeatures("Carbon", carbResId, peId, prt.carbVerFeatures)
    Await.ready(Future.sequence(Seq(ppeInsert, silFeatureInserts, carbFeatureInserts)), DEFAULT_DB_TIMEOUT)
  }

  def insertProgramAndPrint(pe: ProgramEntry, ppe: ProgramPrintEntry): Future[Any] = {
    val peID = Await.result(
      db.run((sTables.programEntryTable returning sTables.programEntryTable.map(_.programEntryId)) += pe),
      DEFAULT_DB_TIMEOUT
    )
    db.run(sTables.programPrintEntryTable += ppe.copy(programEntryId = peID))
  }

  def clearDB(): Future[Unit] = {
    val deletions   = sTables.tables map (_.delete)
    val deleteQuery = DBIO.seq(deletions: _*)
    db.run(deleteQuery)
  }

  def clearProgramPrintEntries(): Future[Int] = {
    db.run(sTables.programPrintEntryTable.delete)
  }
}
