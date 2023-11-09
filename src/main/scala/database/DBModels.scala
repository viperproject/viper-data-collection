package database

import dataCollection.{ProgramPrint}
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.jdbc.{PostgresProfile}
import slick.lifted.ProvenShape
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
 * @param programPrint     the fingerprint tree of this program for similarity comparisons
 * @param parseSuccess     whether program was able to be parsed
 * @param hasPreamble      whether programs contains global predicates, domains, fields or extensions */
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

  /** returns whether this entry is close enough to another to count as a duplicate.
   *
   * Fields checked for equality: Frontend, Verifier, numbers of methods and functions
   *
   * Fields checked for similarity: loc, args, programPrint */
  def isSimilarTo(other: ProgramEntry): Boolean = {
    lazy val similarLength = this.loc <= 1.2 * other.loc && this.loc >= 0.8 * other.loc
    lazy val sameFrontend = this.frontend == other.frontend
    lazy val sameVerifier = this.originalVerifier == other.originalVerifier
    lazy val similarArgs = this.args.toSet.intersect(other.args.toSet).size >= 0.8 * this.args.size
    lazy val sameNumMethFunc = this.programPrint.numMethods == other.programPrint.numMethods && this.programPrint.numFunctions == other.programPrint.numFunctions
    lazy val thisMatchResult = this.programPrint.matchTrees(other.programPrint)
    lazy val otherMatchResult = other.programPrint.matchTrees(this.programPrint)
    lazy val similarTrees = if (this.frontend == "Silicon" || this.frontend == "Carbon") {
      thisMatchResult.totalPercentage >= 85 && otherMatchResult.totalPercentage >= 85
    } else {
      thisMatchResult.funAndMethMatchPercentage >= 85 && otherMatchResult.funAndMethMatchPercentage >= 85
    }
    similarLength && sameFrontend && sameVerifier && similarArgs && sameNumMethFunc && similarTrees
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
 * @param success          whether the program verified on the users device */
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
  def tupled = (CarbonResult.apply _).tupled
}

/** Class to represent the tables of the database
 * Available tables: [[programEntryTable]], [[userSubmissionTable]], [[siliconResultTable]], [[carbonResultTable]] */
class SlickTables(val profile: PostgresProfile) {

  import profile.api._
  import BinarySerializer._

  implicit val pprintColumnType = MappedColumnType.base[ProgramPrint, Array[Byte]](
    pp => serialize(pp),
    ba => deserialize[ProgramPrint](ba)
  )

  implicit val stringSeqColumnType = MappedColumnType.base[Seq[String], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Seq[String]](ba)
  )

  implicit val strLongSeqColumnType = MappedColumnType.base[Seq[(String, Long)], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Seq[(String, Long)]](ba)
  )

  implicit val absErrorSeqColumnType = MappedColumnType.base[Seq[AbstractError], Array[Byte]](
    seq => serialize(seq),
    ba => deserialize[Seq[AbstractError]](ba)
  )

  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntry](tag, Some("programs"), "ProgramEntries") {
    def programEntryId = column[Long]("programEntryId", O.PrimaryKey, O.AutoInc)

    def submissionDate = column[Timestamp]("submissionDate")

    def originalName = column[String]("originalName")

    def program = column[String]("program")

    def loc = column[Int]("loc")

    def frontend = column[String]("frontend")

    def originalVerifier = column[String]("originalVerifier")

    def args = column[Seq[String]]("args")

    def programPrint = column[ProgramPrint]("programPrint")

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
      programPrint,
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

    def args = column[Seq[String]]("argsBlob")

    def originalVerifier = column[String]("originalVerifier")

    def success = column[Boolean]("success")

    override def * : ProvenShape[UserSubmission] = (submissionId,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      args,
      originalVerifier,
      success) <> (UserSubmission.tupled, UserSubmission.unapply)

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

    def errors = column[Seq[AbstractError]]("errors")

    def phaseRuntimes = column[Seq[(String, Long)]]("phaseRuntimes")

    def benchmarkResults = column[Seq[(String, Long)]]("benchmarkResults")

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

    def errors = column[Seq[AbstractError]]("errors")

    def phaseRuntimes = column[Seq[(String, Long)]]("phaseRuntimes")

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


  def getDDL: String = {
    val schema = programEntryTable.schema ++ userSubmissionTable.schema ++ siliconResultTable.schema ++ carbonResultTable.schema
    schema.createIfNotExists.statements.mkString(";\n")
  }
}

object PGSlickTables extends SlickTables(PostgresProfile)


/** Provides helper functions to convert any nullable type to a byte array and back.
 * Used to store more complex types in database */
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
