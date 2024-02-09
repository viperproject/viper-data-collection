package database

import dataCollection.ProgramPrint
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.ast.BaseTypedType
import slick.jdbc.{JdbcType, PostgresProfile}
import slick.lifted.ProvenShape
import queryFrontend._

import java.io.{ByteArrayInputStream, ObjectInputStream, ObjectOutputStream}
import java.sql.Timestamp
import scala.reflect.ClassTag

case class ProgramPrintEntry(pprintId: Long, programEntryId: Long, programPrint: ProgramPrint) extends Serializable

object ProgramPrintEntry {
  def tupled = (ProgramPrintEntry.apply _).tupled
}

/** Class to represent the tables of the database
  * Available tables: [[programEntryTable]], [[userSubmissionTable]], [[siliconResultTable]], [[carbonResultTable]]
  */
class SlickTables(val profile: PostgresProfile) {

  import profile.api._
  import BinarySerializer._

  private def serializableColumnType[T <: Serializable: ClassTag]: JdbcType[T] with BaseTypedType[T] =
    MappedColumnType.base[T, Array[Byte]](
      t => serialize(t),
      ba => deserialize[T](ba)
    )

  //Implicit converters for column types that can't be stored natively in Postgres

  implicit val pprintColumnType = serializableColumnType[ProgramPrint]

  implicit val stringArrColumnType = serializableColumnType[Array[String]]

  implicit val strLongArrColumnType = serializableColumnType[Array[(String, Long)]]

  implicit val verErrorArrColumnType = serializableColumnType[Array[VerError]]

  lazy val userSubmissionTable    = TableQuery[UserSubmissionTable]
  lazy val programEntryTable      = TableQuery[ProgramEntryTable]
  lazy val siliconResultTable     = TableQuery[SiliconResultTable]
  lazy val carbonResultTable      = TableQuery[CarbonResultTable]
  lazy val programPrintEntryTable = TableQuery[ProgramPrintEntryTable]
  lazy val featureTable           = TableQuery[FeatureTable]
  lazy val silFeatureEntryTable   = TableQuery[SilFeatureEntryTable]
  lazy val carbFeatureEntryTable  = TableQuery[CarbFeatureEntryTable]
  lazy val constFeatureEntryTable = TableQuery[ConstFeatureEntryTable]

  def tables = Seq(
    userSubmissionTable,
    programEntryTable,
    siliconResultTable,
    carbonResultTable,
    programPrintEntryTable,
    featureTable,
    silFeatureEntryTable,
    carbFeatureEntryTable,
    constFeatureEntryTable
  )

  //abbreviation
  private val casc = ForeignKeyAction.Cascade

  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntry](tag, Some("programs"), "ProgramEntries") {
    def programEntryId = column[Long]("programEntryId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def originalVerifier = column[String]("originalVerifier")

    def args = column[Array[String]]("args")

    def originalRuntime = column[Long]("originalRuntime")

    def parseSuccess = column[Boolean]("parseSuccess")

    override def * : ProvenShape[ProgramEntry] = (
      programEntryId,
      submissionDate,
      program,
      loc,
      frontend,
      originalVerifier,
      args,
      originalRuntime,
      parseSuccess
    ) <> (ProgramEntry.tupled, ProgramEntry.unapply)

  }

  class UserSubmissionTable(tag: Tag) extends Table[UserSubmission](tag, Some("programs"), "UserSubmissions") {
    def submissionId = column[Long]("submissionId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def args = column[Array[String]]("argsBlob")

    def originalVerifier = column[String]("originalVerifier")

    def success = column[Boolean]("success")

    def runtime = column[Long]("runtime")

    override def * : ProvenShape[UserSubmission] = (
      submissionId,
      submissionDate,
      program,
      loc,
      frontend,
      args,
      originalVerifier,
      success,
      runtime
    ) <> (UserSubmission.tupled, UserSubmission.unapply)

  }

  class SiliconResultTable(tag: Tag) extends Table[VerResult](tag, Some("programs"), "SiliconResults") {
    def silResId = column[Long]("silResId", O.PrimaryKey, O.AutoInc)

    def creationDate = column[Timestamp]("creationDate")

    def verifierHash = column[String]("verifierHash")

    def programEntryId = column[Long]("programEntryId")

    def programEntry =
      foreignKey("silPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def success = column[Boolean]("success")

    def didTimeout = column[Boolean]("didTimeout")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")

    override def * : ProvenShape[VerResult] = (
      silResId,
      creationDate,
      verifierHash,
      programEntryId,
      success,
      didTimeout,
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

    def programEntry =
      foreignKey("carbPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def success = column[Boolean]("success")

    def didTimeout = column[Boolean]("didTimeout")

    def runtime = column[Long]("runtime")

    def errors = column[Array[VerError]]("errors")

    def phaseRuntimes = column[Array[(String, Long)]]("phaseRuntimes")

    override def * : ProvenShape[VerResult] = (
      carbResId,
      creationDate,
      verifierHash,
      programEntryId,
      success,
      didTimeout,
      runtime,
      errors,
      phaseRuntimes
    ) <> (VerResult.tupled, VerResult.unapply)

  }

  class ProgramPrintEntryTable(tag: Tag) extends Table[ProgramPrintEntry](tag, Some("programs"), "ProgramPrintEntry") {
    def pprintId = column[Long]("pprintID", O.PrimaryKey, O.AutoInc)

    def programEntryId = column[Long]("programEntryId")

    def programEntry =
      foreignKey("pprintPE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def programPrint = column[ProgramPrint]("programPrint")

    override def * : ProvenShape[ProgramPrintEntry] =
      (pprintId, programEntryId, programPrint) <> (ProgramPrintEntry.tupled, ProgramPrintEntry.unapply)

  }

  class FeatureTable(tag: Tag) extends Table[Feature](tag, Some("programs"), "Features") {
    def name = column[String]("name", O.PrimaryKey)

    override def * : ProvenShape[Feature] = name <> (Feature, Feature.unapply)

  }

  class SilFeatureEntryTable(tag: Tag) extends Table[FeatureEntry](tag, Some("programs"), "SiliconFeatureEntries") {
    def featureEntryId = column[Long]("silFeatureEntryId", O.PrimaryKey, O.AutoInc)

    def featureName = column[String]("featureName")

    def feature = foreignKey("sfeF_FK", featureName, featureTable)(_.name, onDelete = casc, onUpdate = casc)

    def resultId = column[Long]("resultId")

    def result = foreignKey("sfeSR_FK", resultId, siliconResultTable)(_.silResId, onDelete = casc, onUpdate = casc)

    def value = column[String]("value")

    override def * : ProvenShape[FeatureEntry] =
      (featureEntryId, featureName, resultId, value) <> (FeatureEntry.tupled, FeatureEntry.unapply)

  }

  class CarbFeatureEntryTable(tag: Tag) extends Table[FeatureEntry](tag, Some("programs"), "CarbonFeatureEntries") {
    def featureEntryId = column[Long]("carbFeatureEntryId", O.PrimaryKey, O.AutoInc)

    def featureName = column[String]("featureName")

    def feature = foreignKey("cfeF_FK", featureName, featureTable)(_.name, onDelete = casc, onUpdate = casc)

    def resultId = column[Long]("resultId")

    def result = foreignKey("cfeSR_FK", resultId, carbonResultTable)(_.carbResId, onDelete = casc, onUpdate = casc)

    def value = column[String]("value")

    override def * : ProvenShape[FeatureEntry] =
      (featureEntryId, featureName, resultId, value) <> (FeatureEntry.tupled, FeatureEntry.unapply)

  }

  class ConstFeatureEntryTable(tag: Tag) extends Table[FeatureEntry](tag, Some("programs"), "ConstFeatureEntries") {
    def featureEntryId = column[Long]("constFeatureEntryId", O.PrimaryKey, O.AutoInc)

    def featureName = column[String]("featureName")

    def feature = foreignKey("constfeF_FK", featureName, featureTable)(_.name, onDelete = casc, onUpdate = casc)

    def programEntryId = column[Long]("programEntryId")

    def result = foreignKey("constfePE_FK", programEntryId, programEntryTable)(_.programEntryId, onDelete = casc, onUpdate = casc)

    def value = column[String]("value")

    override def * : ProvenShape[FeatureEntry] =
      (featureEntryId, featureName, programEntryId, value) <> (FeatureEntry.tupled, FeatureEntry.unapply)

  }

  def getDDL: String = {
    val schema = (tables map (_.schema)).reduce((s1, s2) => s1 ++ s2)
    schema.createIfNotExistsStatements.mkString(";\n")
  }
}

/** Database tables for a generic Postgres Profile */
object PGSlickTables extends SlickTables(PostgresProfile)

/** Provides helper functions to convert any nullable type to a byte array and back.
  * Used to store more complex types in database
  */
object BinarySerializer {

  /** Takes serializable object and converts it to Array[Byte] */
  def serialize[T <: Serializable](value: T): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val outStream                     = new ObjectOutputStream(stream)
    outStream.writeObject(value)
    outStream.close()
    stream.toByteArray
  }

  /** Takes Array[Byte] and tries to convert it into [[T]]
    *
    * @param bytes byte representation of object to deserialize
    * @return Either deserialized object or null in case of an exception
    */
  def deserialize[T <: Serializable](bytes: Array[Byte]): T = {
    try {
      val inputStream = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val value       = inputStream.readObject.asInstanceOf[T]
      inputStream.close()
      value
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        null.asInstanceOf[T]
    }
  }
}
