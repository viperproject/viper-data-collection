package dataCollection

import viper.silver.parser.{Nodes, PExists, PForPerm, PForall, PNode, PProgram, PQuantifier}
import slick.jdbc.MySQLProfile.api._
import slick.lifted.ProvenShape
case class ProgramEntry(id: Long,
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


class ProgramEntryTable(tag: Tag) extends Table[ProgramEntry](tag, Some("programs"), "ProgramEntries") {
  private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  private def originalName = column[String]("originalName")

  private def program = column[String]("program")

  private def loc = column[Int]("loc")

  private def frontend = column[String]("frontend")

  private def originalVerifier = column[String]("originalVerifier")

  private def siliconResJSON = column[String]("siliconResJSON")

  private def carbonResJSON = column[String]("carbonResJSON")

  private def argsJSON = column[String]("flags")

  private def siliconPhaseRuntimesJSON = column[String]("siliconPhaseRuntimesJSON")

  private def carbonPhaseRuntimesJSON = column[String]("carbonPhaseRuntimesJSON")

  private def programPrintJSON = column[String]("programPrintJSON")

  private def parseSuccess = column[Boolean]("parseSuccess")

  private def hasPreamble = column[Boolean]("hasPreamble")

  override def * : ProvenShape[ProgramEntry] = (id,
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
    hasPreamble) <> (ProgramEntry.tupled, ProgramEntry.unapply)

}

case class UserSubmission(id: Long,
                          originalName: String,
                          program: String,
                          loc: Int,
                          frontend: String,
                          argsJSON: String,
                          originalVerifier: String,
                          success: Boolean)

class UserSubmissionTable(tag: Tag) extends Table[UserSubmission](tag, Some("programs"), "UserSubmissions") {
  private def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  private def originalName = column[String]("originalName")

  private def program = column[String]("program")

  private def loc = column[Int]("loc")

  private def frontend = column[String]("frontend")

  private def argsJSON = column[String]("flags")

  private def originalVerifier = column[String]("originalVerifier")

  private def success = column[Boolean]("success")

  override def * : ProvenShape[UserSubmission] = (id,
    originalName,
    program,
    loc,
    frontend,
    argsJSON,
    originalVerifier,
    success) <> (UserSubmission.tupled, UserSubmission.unapply)

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
