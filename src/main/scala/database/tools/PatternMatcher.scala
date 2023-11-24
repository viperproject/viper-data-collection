package database.tools

import database.DBExecContext._
import database.{DBQueryInterface, ProgramEntry}
import slick.basic.DatabasePublisher
import util.Config._

import java.util.regex.Pattern
import scala.concurrent.{Await, ExecutionContext, Future}

/** Contains methods to search the database for regex patterns */
object PatternMatcher {

  /** Tries to match the given regex to all programs in the database
   *
   * @return list of [[PatternMatchResult]] containing the programEntryIds and line indices of matches */
  def matchRegexAgainstDatabase(regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr)
    val programEntryPublisher: DatabasePublisher[ProgramEntry] = DBQueryInterface.getAllProgramEntriesBatched()
    val matchResultPublisher = programEntryPublisher mapResult {
      pe => {
        val matchIndices = matchRegexOnProgram(pe.program, pattern)
        matchIndices map {
          case Seq() => None
          case res: Seq[Int] => Some(PatternMatchResult(pe.programEntryId, res))
        }
      }
    }
    var matchResults = Seq[Future[Option[PatternMatchResult]]]()
    val appendAction = matchResultPublisher foreach { r => matchResults = matchResults :+ r }
    Await.ready(appendAction, LONG_TIMEOUT)
    Await.result(Future.sequence(matchResults), LONG_TIMEOUT).flatten
  }

  /** Only for testing purposes */
  def matchRegexOnPrograms(programs: Seq[String], regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr, Pattern.CASE_INSENSITIVE)
    var futureResults = Seq[Future[PatternMatchResult]]()

    programs foreach {
      program => futureResults = futureResults :+ (matchRegexOnProgram(program, pattern) map (r => PatternMatchResult(0, r)))
    }

    val results = Await.result(Future.sequence(futureResults), LONG_TIMEOUT)
    results
  }

  /** @param program program to search for the pattern
   * @param pattern  precompiled regex pattern
   * @return future list of lines where pattern was matched */
  private def matchRegexOnProgram(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Seq[Int]] = {
    Future {
      val matcher = pattern.matcher(program)
      var matchIndices = Seq[Int]()
      while (matcher.find()) {
        matchIndices = matchIndices :+ matcher.start()
      }
      matchIndices = matchIndices map (m => charIndexToLine(program, m))
      matchIndices
    }
  }

  private def matchRegexByLine(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Seq[Int]] = {
    Future {
      val matcher = pattern.matcher("")
      var matchIndices = Seq[Int]()
      program.split("\n").zipWithIndex foreach {
        case (line, ind) => if (matcher.reset(line).find()) matchIndices = matchIndices :+ ind
      }
      matchIndices
    }
  }

  private def charIndexToLine(str: String, index: Int) = {
    str.substring(0, index).count(_ == '\n') + 1
  }
}

/** Result of matching regex to a program
 *
 * @param programEntryId the program in which a match occurred
 * @param matchIndices   the line numbers indicating the start regions of the regex match */
case class PatternMatchResult(programEntryId: Long, matchIndices: Seq[Int])

