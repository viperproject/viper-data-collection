package database

import dataCollection.ProgramPrint
import util._
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.ast.BaseTypedType
import slick.jdbc.{JdbcType, PostgresProfile}
import slick.lifted.ProvenShape
import upickle.default.{macroRW, ReadWriter => RW}
import viper.silver.parser._
import viper.silver.verifier.AbstractError

import java.io.{ByteArrayInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.sql.Timestamp


/** Case class to represent a row in the programs.ProgramEntries table of the database
 *
 * @param programEntryId   unique identifier for the entry
 * @param submissionDate   time when this entry was created
 * @param originalName     the file name of the original viper program
 * @param program          the viper program in plaintext
 * @param loc              number of lines of code
 * @param frontend         Viper frontend that produced this program
 * @param originalVerifier Verifier through which program was originally verified - Silicon or Carbon
 * @param args             the arguments originally passed to the verifier
 * @param parseSuccess     whether program was able to be parsed
 * @param hasPreamble      whether programs contains global predicates, domains, fields or extensions */
case class ProgramEntry(programEntryId: Long,
                        submissionDate: Timestamp,
                        originalName: String,
                        program: String,
                        loc: Int,
                        frontend: String,
                        originalVerifier: String,
                        args: Array[String],
                        parseSuccess: Boolean,
                        hasPreamble: Boolean) extends Similarity[ProgramEntry] with Serializable {

  /** returns whether this entry is close enough to another to count as a duplicate.
   *
   * Fields checked for equality: Frontend, Verifier, numbers of methods and functions
   *
   * Fields checked for similarity: loc, args, programPrint */
  def isSimilarTo(other: ProgramEntry): Boolean = {
    lazy val similarLength = this.loc <= 1.2 * other.loc && this.loc >= 0.8 * other.loc
    lazy val sameFrontend = this.frontend == other.frontend
    lazy val sameVerifier = this.originalVerifier == other.originalVerifier
    lazy val similarArgs = this.args.toSet.intersect(other.args.toSet).size >= 0.8 * this.args.length
    similarLength && sameFrontend && sameVerifier && similarArgs
  }
}

object ProgramEntry {

  def tupled = (ProgramEntry.apply _).tupled
}


/** Case class to represent a row in the programs.UserSubmissions table of the database
 *
 * @param submissionId     unique identifier for the entry
 * @param submissionDate   time when this entry was created
 * @param originalName     the file name of the original viper program
 * @param program          the viper program in plaintext
 * @param loc              number of lines of code
 * @param frontend         Viper frontend that produced this program
 * @param originalVerifier Verifier through which program was originally verified - Silicon or Carbon
 * @param args             the arguments originally passed to the verifier
 * @param success          whether the program verified on the users device
 * @param runtime          how long it took the user's verifier to finish */
case class UserSubmission(submissionId: Long,
                          submissionDate: Timestamp,
                          originalName: String,
                          program: String,
                          loc: Int,
                          frontend: String,
                          args: Array[String],
                          originalVerifier: String,
                          success: Boolean,
                          runtime: Long) extends Serializable

object UserSubmission {

  def tupled = (UserSubmission.apply _).tupled
}

/** Case class to represent a row in the programs.SiliconResults table of the database
 *
 * @param silResId         unique identifier for the entry
 * @param creationDate     time when this entry was created
 * @param siliconHash      commit hash of the silicon version used to get this result
 * @param programEntryId   id referring to the ProgramEntry that was profiled
 * @param success          whether program verified successfully
 * @param runtime          total time for verification
 * @param errors           errors encountered during verification - should be empty if [[success]]
 * @param phaseRuntimes    runtimes of the phases of silicon
 * @param benchmarkResults more detailed information by the [[viper.silver.reporter.BenchmarkingReporter]] */
case class SiliconResult(silResId: Long,
                         creationDate: Timestamp,
                         siliconHash: String,
                         programEntryId: Long,
                         success: Boolean,
                         runtime: Long,
                         errors: Array[VerError],
                         phaseRuntimes: Array[(String, Long)],
                         benchmarkResults: Array[(String, Long)]) extends Similarity[SiliconResult] with Serializable {

  /** @return [[true]] if results have the same success, errors and their runtimes are within 50% of each other,
   *          [[false]] else */
  def isSimilarTo(other: SiliconResult): Boolean = {
    lazy val sameRes: Boolean = if (this.success) {
      other.success
    } else {
      val errorIds = Set(this.errors map (_.fullId))
      val otherErrorIds = Set(other.errors map (_.fullId))
      !other.success && errorIds == otherErrorIds
    }
    lazy val similarTime = this.runtime <= other.runtime * 1.5 && this.runtime >= other.runtime / 1.5
    similarTime && sameRes
  }
}

object SiliconResult {

  import BinarySerializer._

  def tupled = (SiliconResult.apply _).tupled
}

/** Case class to represent a row in the programs.CarbonResults table of the database
 *
 * @param carbResId      unique identifier for the entry
 * @param creationDate   time when this entry was created
 * @param carbonHash     commit hash of the carbon version used to get this result
 * @param programEntryId id referring to the ProgramEntry that was profiled
 * @param success        whether program verified successfully
 * @param runtime        total time for verification
 * @param errors         errors encountered during verification - should be empty if [[success]]
 * @param phaseRuntimes  runtimes of the phases of carbon */
case class CarbonResult(carbResId: Long,
                        creationDate: Timestamp,
                        carbonHash: String,
                        programEntryId: Long,
                        success: Boolean,
                        runtime: Long,
                        errors: Array[VerError],
                        phaseRuntimes: Array[(String, Long)]) extends Similarity[CarbonResult] with Serializable {

  /** @return [[true]] if results have the same success, errors and their runtimes are within 50% of each other,
   *          [[false]] else */
  def isSimilarTo(other: CarbonResult): Boolean = {
    lazy val sameRes: Boolean = if (this.success) {
      other.success
    } else {
      val errorIds = Set(this.errors map (_.fullId))
      val otherErrorIds = Set(other.errors map (_.fullId))
      !other.success && errorIds == otherErrorIds
    }
    lazy val similarTime = this.runtime <= other.runtime * 1.5 && this.runtime >= other.runtime / 1.5
    similarTime && sameRes
  }
}

object CarbonResult {
  def tupled = (CarbonResult.apply _).tupled
}

case class ProgramPrintEntry(pprintId: Long,
                             programEntryId: Long,
                             programPrint: ProgramPrint) extends Serializable

object ProgramPrintEntry {
  def tupled = (ProgramPrintEntry.apply _).tupled
}

/** A wrapper class for an [[AbstractError]] to facilitate comparison and serialization and remove unneeded information
 * Comparison is only done through [[fullId]], since [[message]]s are too specific to a given program
 *
 * @param fullId  the original error ID
 * @param message describes the error in full */
case class VerError(fullId: String, message: String) {
  override def equals(obj: Any): Boolean = obj match {
    case that: VerError => this.fullId == that.fullId
    case _ => false
  }

  override def hashCode(): Int = fullId.hashCode
}

object VerError {
  implicit val rw: RW[VerError] = macroRW

  def toError(ae: AbstractError): VerError = {
    VerError(ae.fullId, ae.readableMessage)
  }
}

/** Class to represent the tables of the database
 * Available tables: [[programEntryTable]], [[userSubmissionTable]], [[siliconResultTable]], [[carbonResultTable]] */
class SlickTables(val profile: PostgresProfile) {

  import profile.api._
  import BinarySerializer._

  //Implicit converters for column types that can't be stored natively in Postgres

  implicit val pprintColumnType: JdbcType[ProgramPrint] with BaseTypedType[ProgramPrint] = MappedColumnType.base[ProgramPrint, Array[Byte]](
    pp => serialize(pp),
    ba => deserialize[ProgramPrint](ba)
  )

  implicit val stringArrColumnType: JdbcType[Array[String]] with BaseTypedType[Array[String]] = MappedColumnType.base[Array[String], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Array[String]](ba)
  )

  implicit val strLongArrColumnType: JdbcType[Array[(String, Long)]] with BaseTypedType[Array[(String, Long)]] = MappedColumnType.base[Array[(String, Long)], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Array[(String, Long)]](ba)
  )

  implicit val verErrorArrColumnType: JdbcType[Array[VerError]] with BaseTypedType[Array[VerError]] = MappedColumnType.base[Array[VerError], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Array[VerError]](ba)
  )

  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntry](tag, Some("programs"), "ProgramEntries") {
    def programEntryId = column[Long]("programEntryId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def originalVerifier = column[String]("originalVerifier")

    def args = column[Array[String]]("args")

    def parseSuccess = column[Boolean]("parseSuccess")

    def hasPreamble = column[Boolean]("hasPreamble")

    override def * : ProvenShape[ProgramEntry] = (programEntryId,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      originalVerifier,
      args,
      parseSuccess,
      hasPreamble) <> (ProgramEntry.tupled, ProgramEntry.unapply)

  }

  lazy val programEntryTable = TableQuery[ProgramEntryTable]

  class UserSubmissionTable(tag: Tag) extends Table[UserSubmission](tag, Some("programs"), "UserSubmissions") {
    def submissionId = column[Long]("submissionId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def args = column[Array[String]]("argsBlob")

    def originalVerifier = column[String]("originalVerifier")

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    override def * : ProvenShape[UserSubmission] = (submissionId,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      args,
      originalVerifier,
      success,
      runtime) <> (UserSubmission.tupled, UserSubmission.unapply)

  }

  lazy val userSubmissionTable = TableQuery[UserSubmissionTable]

  class SiliconResultTable(tag: Tag) extends Table[SiliconResult](tag, Some("programs"), "SiliconResults") {
    def silResId = column[Long]("silResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def siliconHash = column[String]("siliconHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("silPE_FK", programEntryId, programEntryTable)(_.programEntryId)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")

    def benchmarkResults = column[Array[(String, Long)]]("benchmarkResults")

    override def * : ProvenShape[SiliconResult] = (silResId,
      creationDate,
      siliconHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes,
      benchmarkResults
    ) <> (SiliconResult.tupled, SiliconResult.unapply)

  }

  lazy val siliconResultTable = TableQuery[SiliconResultTable]


  class CarbonResultTable(tag: Tag) extends Table[CarbonResult](tag, Some("programs"), "CarbonResults") {
    def carbResId = column[Long]("carbResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def carbonHash = column[String]("carbonHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("carbPE_FK", programEntryId, programEntryTable)(_.programEntryId)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")

    override def * : ProvenShape[CarbonResult] = (carbResId,
      creationDate,
      carbonHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes
    ) <> (CarbonResult.tupled, CarbonResult.unapply)

  }

  lazy val carbonResultTable = TableQuery[CarbonResultTable]


  class ProgramPrintEntryTable(tag: Tag) extends Table[ProgramPrintEntry](tag, Some("programs"), "ProgramPrintEntry") {
    def pprintId = column[Long]("pprintID", O.PrimaryKey, O.AutoInc)

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("pprintPE_FK", programEntryId, programEntryTable)(_.programEntryId)


    def programPrint = column[ProgramPrint]("programPrint")

    override def * : ProvenShape[ProgramPrintEntry] = (pprintId,
      programEntryId,
      programPrint
    ) <> (ProgramPrintEntry.tupled, ProgramPrintEntry.unapply)

  }

  lazy val programPrintEntryTable = TableQuery[ProgramPrintEntryTable]


  def getDDL: String = {
    val schema = programEntryTable.schema ++ userSubmissionTable.schema ++ siliconResultTable.schema ++ carbonResultTable.schema ++ programPrintEntryTable.schema
    schema.createIfNotExistsStatements.mkString(";\n")
  }
}

/** Database tables for a generic Postgres Profile */
object PGSlickTables extends SlickTables(PostgresProfile)


/** Provides helper functions to convert any nullable type to a byte array and back.
 * Used to store more complex types in database */
object BinarySerializer {

  /** Takes serializable object and converts it to Array[Byte] */
  def serialize[T <: Serializable](value: T): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val outStream = new ObjectOutputStream(stream)
    outStream.writeObject(value)
    outStream.close()
    stream.toByteArray
  }

  /** Takes Array[Byte] and tries to convert it into [[T]]
   *
   * @param bytes byte representation of object to deserialize
   * @return Either deserialized object or null in case of an exception */
  def deserialize[T <: Serializable](bytes: Array[Byte]): T = {
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

  def hasPreamble(pp: PProgram): Boolean = {
    (Seq(pp.predicates, pp.functions, pp.fields, pp.domains, pp.extensions) map (l => l == List())).exists(identity)
  }

  def parsedSuccessfully(pp: PProgram): Boolean = pp.errors match {
    case List() => true
    case _ => false
  }

}
