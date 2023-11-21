package dataCollection

import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.concurrent.{Await, ExecutionContext, Future}
import database.DBExecContext._
import database.{DBQueryInterface, ProgramEntry}
import slick.basic.DatabasePublisher
import util.Config._

import scala.concurrent.duration.{Duration, SECONDS}

object PatternMatcher {

  def matchRegexAgainstDatabase(regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr)
    val programEntryPublisher: DatabasePublisher[ProgramEntry] = DBQueryInterface.getAllProgramEntriesBatched()
    val matchResultPublisher = programEntryPublisher mapResult {
      pe => {
        val matchIndices = matchRegex(pe.program, pattern)
        matchIndices map {
          case Seq() => None
          case res: Seq[Int] => Some(PatternMatchResult(pe.programEntryId, res))
        }
      }
    }
    var matchResults = Seq[Future[Option[PatternMatchResult]]]()
    val appendAction = matchResultPublisher foreach {r => matchResults = matchResults :+ r}
    Await.ready(appendAction, LONG_TIMEOUT)
    Await.result(Future.sequence(matchResults), LONG_TIMEOUT).flatten
  }

  /** Only for testing purposes */
  def matchProgramsToRegex(programs: Seq[String], regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr, Pattern.CASE_INSENSITIVE)
    var futureResults = Seq[Future[PatternMatchResult]]()

    programs foreach {
      program => futureResults = futureResults :+ (matchRegex(program, pattern) map (r => PatternMatchResult(0, r)))
    }

    val results = Await.result(Future.sequence(futureResults), LONG_TIMEOUT)
    results
  }

  private def matchRegex(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Seq[Int]] = {
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

case class PatternMatchResult(programEntryId: Long, matchIndices: Seq[Int])

