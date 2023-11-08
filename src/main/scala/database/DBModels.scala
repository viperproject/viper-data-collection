package database

import dataCollection.{ProgramPrint, VerifierResult}
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.jdbc.MySQLProfile
import slick.lifted.ProvenShape
import viper.silver.parser._
import viper.silver.verifier.AbstractError

import java.io.{ByteArrayInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.sql.Timestamp

case class ProgramEntry(programEntryId: Long,
                        submissionDate: Timestamp,
                        originalName: String,
                        program: String,
                        loc: Int,
                        frontend: String,
                        originalVerifier: String,
                        args: Seq[String],
                        programPrint: ProgramPrint,
                        parseSuccess: Boolean,
                        hasPreamble: Boolean) {

  def isSimilarTo(other: ProgramEntry): Boolean = {
    lazy val similarLength = this.loc <= 1.2 * other.loc && this.loc >= 0.8 * other.loc
    lazy val sameFrontend = this.frontend == other.frontend
    lazy val sameVerifier = this.originalVerifier == other.originalVerifier
    lazy val similarArgs = this.args.toSet.intersect(other.args.toSet).size >= 0.8 * this.args.size
    lazy val thisMatchResult = this.programPrint.matchTrees(other.programPrint)
    lazy val otherMatchResult = other.programPrint.matchTrees(this.programPrint)
    lazy val similarTrees = if (this.frontend == "Silicon" || this.frontend == "Carbon") {
      thisMatchResult.totalPercentage >= 85 && otherMatchResult.totalPercentage >= 85
    } else {
      thisMatchResult.funAndMethMatchPercentage >= 85 && otherMatchResult.funAndMethMatchPercentage >= 85
    }
    similarLength && sameFrontend && sameVerifier && similarArgs && similarTrees
  }
}

object ProgramEntry {
  def toBlob(pE: ProgramEntry): ProgramEntryBlob = {
    import BinarySerializer._
    ProgramEntryBlob(pE.programEntryId,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      serialize(pE.args),
      serialize(pE.programPrint),
      pE.parseSuccess,
      pE.hasPreamble)
  }

  def deBlob(pE: ProgramEntryBlob): ProgramEntry = {
    import BinarySerializer._
    database.ProgramEntry(pE.programEntryId,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      deserialize[Seq[String]](pE.argsBlob),
      deserialize[ProgramPrint](pE.programPrintBlob),
      pE.parseSuccess,
      pE.hasPreamble)
  }
}

case class ProgramEntryBlob(programEntryId: Long,
                            submissionDate: Timestamp,
                            originalName: String,
                            program: String,
                            loc: Int,
                            frontend: String,
                            originalVerifier: String,
                            argsBlob: Array[Byte],
                            programPrintBlob: Array[Byte],
                            parseSuccess: Boolean,
                            hasPreamble: Boolean)


case class UserSubmission(submissionId: Long,
                          submissionDate: Timestamp,
                          originalName: String,
                          program: String,
                          loc: Int,
                          frontend: String,
                          args: Seq[String],
                          originalVerifier: String,
                          success: Boolean)

object UserSubmission {
  def toBlob(uS: UserSubmission): UserSubmissionBlob = {
    import BinarySerializer._
    UserSubmissionBlob(uS.submissionId,
      uS.submissionDate,
      uS.originalName,
      uS.program,
      uS.loc,
      uS.frontend,
      serialize(uS.args),
      uS.originalVerifier,
      uS.success)
  }

  def deBlob(uS: UserSubmissionBlob): UserSubmission = {
    import BinarySerializer._
    UserSubmission(uS.submissionId,
      uS.submissionDate,
      uS.originalName,
      uS.program,
      uS.loc,
      uS.frontend,
      deserialize[Seq[String]](uS.argsBlob),
      uS.originalVerifier,
      uS.success)
  }
}

case class UserSubmissionBlob(submissionId: Long,
                              submissionDate: Timestamp,
                              originalName: String,
                              program: String,
                              loc: Int,
                              frontend: String,
                              argsBlob: Array[Byte],
                              originalVerifier: String,
                              success: Boolean)


case class SiliconResult(silResId: Long,
                         creationDate: Timestamp,
                         siliconHash: String,
                         programEntryId: Long,
                         success: Boolean,
                         runtime: Long,
                         errors: Seq[AbstractError],
                         phaseRuntimes: Seq[(String, Long)],
                         benchmarkResults: Seq[(String, Long)]) {
  def isSimilarTo(other: SiliconResult, timeEps: Double): Boolean = {
    lazy val sameRes: Boolean = if (this.success) {
      other.success
    } else {
      val errorIds = Set(this.errors map (_.fullId))
      val otherErrorIds = Set(other.errors map (_.fullId))
      !other.success && errorIds == otherErrorIds
    }
    lazy val similarTime = (this.runtime <= other.runtime * timeEps && this.runtime >= other.runtime / timeEps)
    similarTime && sameRes
  }
}

object SiliconResult {

  import BinarySerializer._

  def toBlob(sr: SiliconResult): SiliconResultBlob = {

    SiliconResultBlob(
      sr.silResId,
      sr.creationDate,
      sr.siliconHash,
      sr.programEntryId,
      sr.success,
      sr.runtime,
      serialize(sr.errors),
      serialize(sr.phaseRuntimes),
      serialize((sr.benchmarkResults))
    )
  }

  def deBlob(sr: SiliconResultBlob): SiliconResult = {
    SiliconResult(
      sr.silResId,
      sr.creationDate,
      sr.siliconHash,
      sr.programEntryId,
      sr.success,
      sr.runtime,
      deserialize[Seq[AbstractError]](sr.errors),
      deserialize[Seq[(String, Long)]](sr.phaseRuntimesBlob),
      deserialize[Seq[(String, Long)]](sr.benchmarkResultsBlob)
    )
  }
}

case class SiliconResultBlob(silResId: Long,
                             creationDate: Timestamp,
                             siliconHash: String,
                             programEntryId: Long,
                             success: Boolean,
                             runtime: Long,
                             errors: Array[Byte],
                             phaseRuntimesBlob: Array[Byte],
                             benchmarkResultsBlob: Array[Byte])


case class CarbonResult(carbResId: Long,
                        creationDate: Timestamp,
                        carbonHash: String,
                        programEntryId: Long,
                        success: Boolean,
                        runtime: Long,
                        errors: Seq[AbstractError],
                        phaseRuntimes: Seq[(String, Long)]) {
  def isSimilarTo(other: CarbonResult, timeEps: Double): Boolean = {
    lazy val sameRes: Boolean = if (this.success) {
      other.success
    } else {
      val errorIds = Set(this.errors map (_.fullId))
      val otherErrorIds = Set(other.errors map (_.fullId))
      !other.success && errorIds == otherErrorIds
    }
    lazy val similarTime = (this.runtime <= other.runtime * timeEps && this.runtime >= other.runtime / timeEps)
    similarTime && sameRes
  }
}

object CarbonResult {

  import BinarySerializer._

  def toBlob(sr: CarbonResult): CarbonResultBlob = {

    CarbonResultBlob(
      sr.carbResId,
      sr.creationDate,
      sr.carbonHash,
      sr.programEntryId,
      sr.success,
      sr.runtime,
      serialize(sr.errors),
      serialize(sr.phaseRuntimes)
    )
  }

  def deBlob(sr: CarbonResultBlob): CarbonResult = {
    CarbonResult(
      sr.carbonResId,
      sr.creationDate,
      sr.carbonHash,
      sr.programEntryId,
      sr.success,
      sr.runtime,
      deserialize[Seq[AbstractError]](sr.errors),
      deserialize[Seq[(String, Long)]](sr.phaseRuntimesBlob)
    )
  }
}

case class CarbonResultBlob(carbonResId: Long,
                            creationDate: Timestamp,
                            carbonHash: String,
                            programEntryId: Long,
                            success: Boolean,
                            runtime: Long,
                            errors: Array[Byte],
                            phaseRuntimesBlob: Array[Byte])

class SlickTables(val profile: MySQLProfile) {
  import profile.api._
  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntryBlob](tag, Some("programs"), "ProgramEntries") {
    def programEntryId = column[Long]("programEntryId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def originalVerifier = column[String]("originalVerifier")

    def argsBlob = column[Array[Byte]]("argsBlob")

    def programPrintBlob = column[Array[Byte]]("programPrintBlob")

    def parseSuccess = column[Boolean]("parseSuccess")

    def hasPreamble = column[Boolean]("hasPreamble")

    override def * : ProvenShape[ProgramEntryBlob] = (programEntryId,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      originalVerifier,
      argsBlob,
      programPrintBlob,
      parseSuccess,
      hasPreamble) <> (ProgramEntryBlob.tupled, ProgramEntryBlob.unapply)

  }

  lazy val programEntryTable = TableQuery[ProgramEntryTable]

  class UserSubmissionTable(tag: Tag) extends Table[UserSubmissionBlob](tag, Some("programs"), "UserSubmissions") {
    def submissionId = column[Long]("submissionId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def argsBlob = column[Array[Byte]]("argsBlob")

    def originalVerifier = column[String]("originalVerifier")

    def success = column[Boolean]("success")

    override def * : ProvenShape[UserSubmissionBlob] = (submissionId,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      argsBlob,
      originalVerifier,
      success) <> (UserSubmissionBlob.tupled, UserSubmissionBlob.unapply)

  }

  lazy val userSubmissionTable = TableQuery[UserSubmissionTable]

  class SiliconResultTable(tag: Tag) extends Table[SiliconResultBlob](tag, Some("programs"), "SiliconResults") {
    def silResId = column[Long]("silResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def siliconHash = column[String]("siliconHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("silPE_FK", programEntryId, programEntryTable)(_.programEntryId)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[Byte]]("errors")

    def phaseRuntimesBlob = column[Array[Byte]]("phaseRuntimesBlob")

    def benchmarkResultsBlob = column[Array[Byte]]("benchmarkResultsBlob")

    override def * : ProvenShape[SiliconResultBlob] = (silResId,
      creationDate,
      siliconHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimesBlob,
      benchmarkResultsBlob
    ) <> (SiliconResultBlob.tupled, SiliconResultBlob.unapply)

  }

  lazy val siliconResultTable = TableQuery[SiliconResultTable]


  class CarbonResultTable(tag: Tag) extends Table[CarbonResultBlob](tag, Some("programs"), "CarbonResults") {
    def carbResId = column[Long]("carbResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def carbonHash = column[String]("carbonHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("carbPE_FK", programEntryId, programEntryTable)(_.programEntryId)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[Byte]]("errors")

    def phaseRuntimesBlob = column[Array[Byte]]("phaseRuntimesBlob")

    override def * : ProvenShape[CarbonResultBlob] = (carbResId,
      creationDate,
      carbonHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimesBlob
    ) <> (CarbonResultBlob.tupled, CarbonResultBlob.unapply)

  }

  lazy val carbonResultTable = TableQuery[CarbonResultTable]


  def getDDL: String = {
    val schema = programEntryTable.schema ++ userSubmissionTable.schema ++ siliconResultTable.schema ++ carbonResultTable.schema
    schema.createIfNotExists.statements.mkString(";\n")
  }
}

object MySQLSlickTables extends SlickTables(MySQLProfile)


object BinarySerializer {
  def serialize(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val outStream = new ObjectOutputStream(stream)
    outStream.writeObject(value)
    outStream.close()
    stream.toByteArray
  }

  def deserialize[T >: Null <: AnyRef](bytes: Array[Byte]): T = {
    try {
      val inputStream = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val value = inputStream.readObject.asInstanceOf[T]
      inputStream.close()
      value
    } catch {
      case cex: ClassNotFoundException => {
        cex.printStackTrace()
        null.asInstanceOf[T]
      }
      case iex: IOException => {
        iex.printStackTrace()
        null.asInstanceOf[T]
      }
    }
  }
}

object FeatureExtractor {

  def pnodeHasQP(pn: PNode): Boolean = pn match {
    case PForall(_, _, _) | PExists(_, _, _) | PForPerm(_, _, _) => true
    case _ => {
      lazy val subTreeRes = Nodes.subnodes(pn) map pnodeHasQP
      subTreeRes.exists(identity)
    }
  }

  def hasPreamble(pp: PProgram) = {
    (Seq(pp.predicates, pp.functions, pp.fields, pp.domains, pp.extensions) map (l => l == List())).exists(identity)
  }

  def parsedSuccessfully(pp: PProgram): Boolean = pp.errors match {
    case List() => true
    case _ => false
  }

}
