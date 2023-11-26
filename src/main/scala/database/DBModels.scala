package database

import dataCollection.ProgramPrint
import util._
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.ast.BaseTypedType
import slick.jdbc.{JdbcType, PostgresProfile}
import slick.lifted.ProvenShape
import upickle.default.{macroRW, ReadWriter => RW}
import viper.silver.verifier.AbstractError

import java.io.{ByteArrayInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.sql.Timestamp
import scala.reflect.ClassTag

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
                        originalRuntime: Long,
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
    lazy val similarArgs = this.args.toSet.filter(_.startsWith("--")) == other.args.toSet.filter(_.startsWith("--"))
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

/** Abstract class to represent the result of running some verifier on a program */
case class VerResult(resId: Long,
                     creationDate: Timestamp,
                     verifierHash: String,
                     programEntryId: Long,
                     success: Boolean,
                     runtime: Long,
                     errors: Array[VerError],
                     phaseRuntimes: Array[(String, Long)]
                    ) extends Similarity[VerResult] with Serializable {

  def isSimilarTo(other: VerResult): Boolean = {
    lazy val sameRes: Boolean = if (this.success) {
      other.success
    } else {
      val errorIds = Set(this.errors map (_.fullId))
      val otherErrorIds = Set(other.errors map (_.fullId))
      !other.success && errorIds == otherErrorIds
    }
    lazy val similarRuntime = similarTime(this.runtime, other.runtime) // either time in +-50% of other or +-2seconds (for variance in small programs)
    similarRuntime && sameRes
  }

  private def similarTime(t1: Long, t2: Long): Boolean = {
    ((t1 <= t2 * 1.5 && t1 >= t2 / 1.5) || (t1 - t2).abs <= 2000)
  }
}

object VerResult {
  def tupled = (VerResult.apply _).tupled
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
                         verifierHash: String,
                         programEntryId: Long,
                         success: Boolean,
                         runtime: Long,
                         errors: Array[VerError],
                         phaseRuntimes: Array[(String, Long)],
                         benchmarkResults: Array[(String, Long)]) //extends VerResult with Serializable

object SiliconResult {
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
                        verifierHash: String,
                        programEntryId: Long,
                        success: Boolean,
                        runtime: Long,
                        errors: Array[VerError],
                        phaseRuntimes: Array[(String, Long)]) //extends VerResult with Serializable

object CarbonResult {
  def tupled = (CarbonResult.apply _).tupled
}

case class ProgramPrintEntry(pprintId: Long,
                             programEntryId: Long,
                             programPrint: ProgramPrint) extends Serializable

object ProgramPrintEntry {
  def tupled = (ProgramPrintEntry.apply _).tupled
}

/** Case class to represent a row in the programs.Features table
 *
 * @param featureId       unique identifier
 * @param name            name of the feature
 * @param useForFiltering whether to use this feature to filter out new entries */
case class Feature(name: String,
                   useForFiltering: Boolean)

object Feature {
  def tupled = (Feature.apply _).tupled
}

/** Case class to represent a row in the programs.silFeatureEntry or carbFeatureEntry table
 *
 * @param featureEntryId unique identifier
 * @param featureId      foreign key for [[Feature]] that is referenced
 * @param resultId       foreign key for [[VerResult]] in which this feature was created
 * @param value          value of the feature */
case class FeatureEntry(featureEntryId: Long,
                        featureName: String,
                        resultId: Long,
                        value: String)

object FeatureEntry {
  def tupled = (FeatureEntry.apply _).tupled
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

  private def serializableColumnType[T <: Serializable : ClassTag]: JdbcType[T] with BaseTypedType[T] = MappedColumnType.base[T, Array[Byte]](
    t => serialize(t),
    ba => deserialize[T](ba)
  )

  //Implicit converters for column types that can't be stored natively in Postgres

  implicit val pprintColumnType = serializableColumnType[ProgramPrint]

  implicit val stringArrColumnType = serializableColumnType[Array[String]]

  implicit val strLongArrColumnType = serializableColumnType[Array[(String, Long)]]

  implicit val verErrorArrColumnType = serializableColumnType[Array[VerError]]

  lazy val userSubmissionTable = TableQuery[UserSubmissionTable]
  lazy val programEntryTable = TableQuery[ProgramEntryTable]
  lazy val siliconResultTable = TableQuery[SiliconResultTable]
  lazy val carbonResultTable = TableQuery[CarbonResultTable]
  lazy val programPrintEntryTable = TableQuery[ProgramPrintEntryTable]
  lazy val featureTable = TableQuery[FeatureTable]
  lazy val silFeatureEntryTable = TableQuery[SilFeatureEntryTable]
  lazy val carbFeatureEntryTable = TableQuery[CarbFeatureEntryTable]

  def tables = Seq(userSubmissionTable, programEntryTable, siliconResultTable, carbonResultTable, programPrintEntryTable,
    featureTable, silFeatureEntryTable, carbFeatureEntryTable)

  //abbreviation
  private val casc = ForeignKeyAction.Cascade

  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntry](tag, Some("programs"), "ProgramEntries") {
    def programEntryId = column[Long]("programEntryId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def originalVerifier = column[String]("originalVerifier")

    def args = column[Array[String]]("args")

    def originalRuntime = column[Long]("originalRuntime")

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
      originalRuntime,
      parseSuccess,
      hasPreamble) <> (ProgramEntry.tupled, ProgramEntry.unapply)

  }

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


  class SiliconResultTable(tag: Tag) extends Table[VerResult](tag, Some("programs"), "SiliconResults") {
    def silResId = column[Long]("silResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def verifierHash = column[String]("verifierHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("silPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")


    override def * : ProvenShape[VerResult] = (silResId,
      creationDate,
      verifierHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes
    ) <> (VerResult.tupled, VerResult.unapply)

  }


  class CarbonResultTable(tag: Tag) extends Table[VerResult](tag, Some("programs"), "CarbonResults") {
    def carbResId = column[Long]("carbResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def verifierHash = column[String]("verifierHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("carbPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")

    override def * : ProvenShape[VerResult] = (carbResId,
      creationDate,
      verifierHash,
      programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes
    ) <> (VerResult.tupled, VerResult.unapply)

  }


  class ProgramPrintEntryTable(tag: Tag) extends Table[ProgramPrintEntry](tag, Some("programs"), "ProgramPrintEntry") {
    def pprintId = column[Long]("pprintID", O.PrimaryKey, O.AutoInc)

    def programEntryId = column[Long]("programEntryId")

    def programEntry = foreignKey("pprintPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)


    def programPrint = column[ProgramPrint]("programPrint")

    override def * : ProvenShape[ProgramPrintEntry] = (pprintId,
      programEntryId,
      programPrint
    ) <> (ProgramPrintEntry.tupled, ProgramPrintEntry.unapply)

  }

  class FeatureTable(tag: Tag) extends Table[Feature](tag, Some("programs"), "Features") {
    def name = column[String]("name", O.PrimaryKey)

    def useForFiltering = column[Boolean]("useForFiltering")

    override def * : ProvenShape[Feature] = (name,
      useForFiltering
    ) <> (Feature.tupled, Feature.unapply)

  }

  class SilFeatureEntryTable(tag: Tag) extends Table[FeatureEntry](tag, Some("programs"), "SiliconFeatureEntries") {
    def featureEntryId = column[Long]("silFeatureEntryId", O.PrimaryKey, O.AutoInc)

    def featureName = column[String]("featureName")

    def feature = foreignKey("sfeF_FK", featureName, featureTable)(_.name, onDelete = casc, onUpdate = casc)

    def resultId = column[Long]("resultId")

    def result = foreignKey("sfeSR_FK", resultId, siliconResultTable)(_.silResId, onDelete = casc, onUpdate = casc)

    def value = column[String]("value")

    override def * : ProvenShape[FeatureEntry] = (featureEntryId,
      featureName,
      resultId,
      value
    ) <> (FeatureEntry.tupled, FeatureEntry.unapply)

  }

  class CarbFeatureEntryTable(tag: Tag) extends Table[FeatureEntry](tag, Some("programs"), "CarbonFeatureEntries") {
    def featureEntryId = column[Long]("carbFeatureEntryId", O.PrimaryKey, O.AutoInc)

    def featureName = column[String]("featureName")

    def feature = foreignKey("cfeF_FK", featureName, featureTable)(_.name, onDelete = casc, onUpdate = casc)

    def resultId = column[Long]("resultId")

    def result = foreignKey("cfeSR_FK", resultId, carbonResultTable)(_.carbResId, onDelete = casc, onUpdate = casc)

    def value = column[String]("value")

    override def * : ProvenShape[FeatureEntry] = (featureEntryId,
      featureName,
      resultId,
      value
    ) <> (FeatureEntry.tupled, FeatureEntry.unapply)

  }


  def getDDL: String = {
    val schema = (tables map (_.schema)).reduce((s1, s2) => s1 ++ s2)
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
      case ex: Exception =>
        ex.printStackTrace()
        null.asInstanceOf[T]
    }
  }
}