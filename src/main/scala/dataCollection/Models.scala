package dataCollection

import upickle.default.{macroRW, read, write, ReadWriter => RW}
import viper.silver.parser.{Nodes, PExists, PForPerm, PForall, PNode, PProgram, PQuantifier}
import slick.jdbc.MySQLProfile.api._
import slick.lifted.ProvenShape

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
                        programPrint: ProgramPrint,
                        parseSuccess: Boolean,
                        hasPreamble: Boolean)

object ProgramEntry {
  def serialize(pE: ProgramEntry): SerializedProgramEntry = {
    SerializedProgramEntry(pE.id,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      write(pE.siliconRes),
      write(pE.carbonRes),
      write(pE.args),
      write(pE.siliconPhaseRuntimes),
      write(pE.carbonPhaseRuntimes),
      write(pE.programPrint),
      pE.parseSuccess,
      pE.hasPreamble)
  }

  def deserialize(pE: SerializedProgramEntry): ProgramEntry = {
    ProgramEntry(pE.id,
      pE.submissionDate,
      pE.originalName,
      pE.program,
      pE.loc,
      pE.frontend,
      pE.originalVerifier,
      read(pE.siliconResJSON),
      read(pE.carbonResJSON),
      read(pE.argsJSON),
      read(pE.siliconPhaseRuntimesJSON),
      read(pE.carbonPhaseRuntimesJSON),
      read(pE.programPrintJSON),
      pE.parseSuccess,
      pE.hasPreamble)
  }
}

case class SerializedProgramEntry(id: Long,
                                  submissionDate: Timestamp,
                                  originalName: String,
                                  program: String,
                                  loc: Int,
                                  frontend: String,
                                  originalVerifier: String,
                                  siliconResJSON: String,
                                  carbonResJSON: String,
                                  argsJSON: String,
                                  siliconPhaseRuntimesJSON: String,
                                  carbonPhaseRuntimesJSON: String,
                                  programPrintJSON: String,
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
  def serialize(uS: UserSubmission): SerializedUserSubmission = {
    SerializedUserSubmission(uS.id,
      uS.submissionDate,
      uS.originalName,
      uS.program,
      uS.loc,
      uS.frontend,
      write(uS.args),
      uS.originalVerifier,
      uS.success)
  }

  def deserialize(uS: SerializedUserSubmission): UserSubmission = {
    UserSubmission(uS.id,
      uS.submissionDate,
      uS.originalName,
      uS.program,
      uS.loc,
      uS.frontend,
      read(uS.argsJSON),
      uS.originalVerifier,
      uS.success)
  }
}

case class SerializedUserSubmission(id: Long,
                                    submissionDate: Timestamp,
                                    originalName: String,
                                    program: String,
                                    loc: Int,
                                    frontend: String,
                                    argsJSON: String,
                                    originalVerifier: String,
                                    success: Boolean)


object SlickTables {
  class ProgramEntryTable(tag: Tag) extends Table[SerializedProgramEntry](tag, Some("programs"), "ProgramEntries") {
    private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    private def submissionDate = column[Timestamp]("submissionDate")

    private def originalName = column[String]("originalName")

    private def program = column[String]("program")

    private def loc = column[Int]("loc")

    private def frontend = column[String]("frontend")

    private def originalVerifier = column[String]("originalVerifier")

    private def siliconResJSON = column[String]("siliconResJSON")

    private def carbonResJSON = column[String]("carbonResJSON")

    private def argsJSON = column[String]("argsJSON")

    private def siliconPhaseRuntimesJSON = column[String]("siliconPhaseRuntimesJSON")

    private def carbonPhaseRuntimesJSON = column[String]("carbonPhaseRuntimesJSON")

    private def programPrintJSON = column[String]("programPrintJSON")

    private def parseSuccess = column[Boolean]("parseSuccess")

    private def hasPreamble = column[Boolean]("hasPreamble")

    override def * : ProvenShape[SerializedProgramEntry] = (id,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      originalVerifier,
      siliconResJSON,
      carbonResJSON,
      argsJSON,
      siliconPhaseRuntimesJSON,
      carbonPhaseRuntimesJSON,
      programPrintJSON,
      parseSuccess,
      hasPreamble) <> (SerializedProgramEntry.tupled, SerializedProgramEntry.unapply)

  }

  lazy val programEntryTable = TableQuery[ProgramEntryTable]

  class UserSubmissionTable(tag: Tag) extends Table[SerializedUserSubmission](tag, Some("programs"), "UserSubmissions") {
    private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    private def submissionDate = column[Timestamp]("submissionDate")

    private def originalName = column[String]("originalName")

    private def program = column[String]("program")

    private def loc = column[Int]("loc")

    private def frontend = column[String]("frontend")

    private def argsJSON = column[String]("argsJSON")

    private def originalVerifier = column[String]("originalVerifier")

    private def success = column[Boolean]("success")

    override def * : ProvenShape[SerializedUserSubmission] = (id,
      submissionDate,
      originalName,
      program,
      loc,
      frontend,
      argsJSON,
      originalVerifier,
      success) <> (SerializedUserSubmission.tupled, SerializedUserSubmission.unapply)

  }

  lazy val userSubmissionTable = TableQuery[UserSubmissionTable]
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
