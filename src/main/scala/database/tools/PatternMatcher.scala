package database.tools

import database.DBExecContext._
import database.{DBQueryInterface}
import queryFrontend._
import slick.basic.DatabasePublisher
import util.Config._

import java.util.regex.{Matcher, Pattern}
import scala.concurrent.{Await, ExecutionContext, Future}

/** Contains methods to search the database for regex patterns */
object PatternMatcher {

  /** Tries to match the given regex to all programs in the database
    *
    * @return list of [[PatternMatchResult]] containing the programEntryIds and line indices of matches
    */
  def matchRegexAgainstDatabase(regexStr: String, flags: Int = 0): Seq[PatternMatchResult] = {
    val pattern: Pattern                                       = Pattern.compile(regexStr, flags)
    val programEntryPublisher: DatabasePublisher[ProgramEntry] = DBQueryInterface.getAllProgramEntriesBatched()
    val matchResultPublisher = programEntryPublisher mapResult { pe =>
      {
        val matchIndices = matchRegexOnProgram(pe.program, pattern)
        matchIndices map {
          case Seq()         => None
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
  def matchRegexOnPrograms(programs: Seq[String], regexStr: String, flags: Int = 0): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr, flags)
    var futureResults    = Seq[Future[PatternMatchResult]]()

    programs foreach { program =>
      futureResults = futureResults :+ (matchRegexOnProgram(program, pattern) map (r => PatternMatchResult(0, r)))
    }

    val results = Await.result(Future.sequence(futureResults), LONG_TIMEOUT)
    results
  }

  def doesMatch(program: String, regexStr: String, flags: Int = 0): Boolean = {
    val matcher: Matcher = Pattern.compile(regexStr, flags).matcher(program)
    matcher.find()
  }

  def matchesAtLeastOne(program: String, regexStrs: Seq[String], flags: Int = 0): Boolean = {
    val matchers = regexStrs map (s => {
      Pattern.compile(s, flags).matcher(program)
    })
    for (m <- matchers) {
      if (m.find()) return true
    }
    false
  }

  /** @param program program to search for the pattern
    * @param pattern  precompiled regex pattern
    * @return future list of lines where pattern was matched
    */
  private def matchRegexOnProgram(program: String, pattern: Pattern)(implicit
    ec: ExecutionContext
  ): Future[Seq[Int]] = {
    Future {
      val matcher      = pattern.matcher(program)
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
      val matcher      = pattern.matcher("")
      var matchIndices = Seq[Int]()
      program.split("\n").zipWithIndex foreach { case (line, ind) =>
        if (matcher.reset(line).find()) matchIndices = matchIndices :+ ind
      }
      matchIndices
    }
  }

  private def charIndexToLine(str: String, index: Int) = {
    str.substring(0, index).count(_ == '\n') + 1
  }

}

