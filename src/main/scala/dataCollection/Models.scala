package dataCollection

import dataCollection.BinarySerializer.serialize
import org.apache.commons.io.output.ByteArrayOutputStream
import slick.jdbc.MySQLProfile
import upickle.default.{macroRW, read, write, ReadWriter => RW}
import viper.silver.parser.{Nodes, PExists, PForPerm, PForall, PNode, PProgram, PQuantifier}
import slick.jdbc.MySQLProfile.api._
import slick.lifted.ProvenShape

import java.io.{ByteArrayInputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.sql.Timestamp

case class ProgramEntry(id: Long,
                        submissionDate: Timestamp,
                        originalName: String,
                        program: String,
                        loc: Int,
                        frontend: String,
                        originalVerifier: String,
                        siliconRes: Option[VerifierResult],
                        carbonRes: Option[VerifierResult],
                        args: Seq[String],
                        siliconPhaseRuntimes: Seq[(String, Long)],
                        carbonPhaseRuntimes: Seq[(String, Long)],
                        siliconBenchmarks: Seq[(String, Long)],
                        programPrint: ProgramPrint,
                        parseSuccess: Boolean,
                        hasPreamble: Boolean)

object ProgramEntry {
  def toBlob(pE: ProgramEntry): ProgramEntryBlob = {
    import BinarySerializer._
    ProgramEntryBlob(pE.id,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      serialize(pE.siliconRes),
      serialize(pE.carbonRes),
      serialize(pE.args),
      serialize(pE.siliconPhaseRuntimes),
      serialize(pE.carbonPhaseRuntimes),
      serialize(pE.siliconBenchmarks),
      serialize(pE.programPrint),
      pE.parseSuccess,
      pE.hasPreamble)
  }

  def deBlob(pE: ProgramEntryBlob): ProgramEntry = {
    import BinarySerializer._
    ProgramEntry(pE.id,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      deserialize[Option[VerifierResult]](pE.siliconResBlob),
      deserialize[Option[VerifierResult]](pE.carbonResBlob),
      deserialize[Seq[String]](pE.argsBlob),
      deserialize[Seq[(String, Long)]](pE.siliconPhaseRuntimesBlob),
      deserialize[Seq[(String, Long)]](pE.carbonPhaseRuntimesBlob),
      deserialize[Seq[(String, Long)]](pE.siliconBenchmarksBlob),
      deserialize[ProgramPrint](pE.programPrintBlob),
      pE.parseSuccess,
      pE.hasPreamble)
  }
}

case class ProgramEntryBlob(id: Long,
                                  submissionDate: Timestamp,
                                  originalName: String,
                                  program: String,
                                  loc: Int,
                                  frontend: String,
                                  originalVerifier: String,
                                  siliconResBlob: Array[Byte],
                                  carbonResBlob: Array[Byte],
                                  argsBlob: Array[Byte],
                                  siliconPhaseRuntimesBlob: Array[Byte],
                                  carbonPhaseRuntimesBlob: Array[Byte],
                                  siliconBenchmarksBlob: Array[Byte],
                                  programPrintBlob: Array[Byte],
                                  parseSuccess: Boolean,
                                  hasPreamble: Boolean)


case class UserSubmission(id: Long,
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
    UserSubmissionBlob(uS.id,
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
    UserSubmission(uS.id,
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

case class UserSubmissionBlob(id: Long,
                                    submissionDate: Timestamp,
                                    originalName: String,
                                    program: String,
                                    loc: Int,
                                    frontend: String,
                                    argsBlob: Array[Byte],
                                    originalVerifier: String,
                                    success: Boolean)


class SlickTables(val profile: MySQLProfile){
  class ProgramEntryTable(tag: Tag) extends Table[ProgramEntryBlob](tag, Some("programs"), "ProgramEntries") {
    private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    private def submissionDate = column[Timestamp]("submissionDate")

    private def originalName = column[String]("originalName")

    private def program = column[String]("program")

    private def loc = column[Int]("loc")

    private def frontend = column[String]("frontend")

    private def originalVerifier = column[String]("originalVerifier")

    private def siliconResBlob = column[Array[Byte]]("siliconResBlob")

    private def carbonResBlob = column[Array[Byte]]("carbonResBlob")

    private def argsBlob = column[Array[Byte]]("argsBlob")

    private def siliconPhaseRuntimesBlob = column[Array[Byte]]("siliconPhaseRuntimesBlob")

    private def carbonPhaseRuntimesBlob = column[Array[Byte]]("carbonPhaseRuntimesBlob")

    private def siliconBenchmarksBlob = column[Array[Byte]]("siliconBenchmarksBlob")

    private def programPrintBlob = column[Array[Byte]]("programPrintBlob")

    private def parseSuccess = column[Boolean]("parseSuccess")

    private def hasPreamble = column[Boolean]("hasPreamble")

    override def * : ProvenShape[ProgramEntryBlob] = (id,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      originalVerifier,
      siliconResBlob,
      carbonResBlob,
      argsBlob,
      siliconPhaseRuntimesBlob,
      carbonPhaseRuntimesBlob,
      siliconBenchmarksBlob,
      programPrintBlob,
      parseSuccess,
      hasPreamble) <> (ProgramEntryBlob.tupled, ProgramEntryBlob.unapply)

  }

  lazy val programEntryTable = TableQuery[ProgramEntryTable]

  class UserSubmissionTable(tag: Tag) extends Table[UserSubmissionBlob](tag, Some("programs"), "UserSubmissions") {
    private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    private def submissionDate = column[Timestamp]("submissionDate")

    private def originalName = column[String]("originalName")

    private def program = column[String]("program")

    private def loc = column[Int]("loc")

    private def frontend = column[String]("frontend")

    private def argsBlob = column[Array[Byte]]("argsBlob")

    private def originalVerifier = column[String]("originalVerifier")

    private def success = column[Boolean]("success")

    override def * : ProvenShape[UserSubmissionBlob] = (id,
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
}

object GenericSlickTables extends SlickTables(MySQLProfile)


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
