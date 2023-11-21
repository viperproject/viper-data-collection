package dataCollection

import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.concurrent.{Await, ExecutionContext, Future}
import database.DBExecContext._

import scala.concurrent.duration.{Duration, SECONDS}

object PatternMatcher {

  def matchProgramsToRegex(programs: Seq[String], regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr, Pattern.CASE_INSENSITIVE)
    var futureResults = Seq[Future[Option[PatternMatchResult]]]()

    programs foreach {
      program => futureResults = futureResults :+ matchRegex(program, pattern)
    }

    val results = Await.result(Future.sequence(futureResults), Duration(100, SECONDS)).flatten
    results
  }

  private def matchRegex(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Option[PatternMatchResult]] = {
    Future {
      val matcher = pattern.matcher(program)
      var matchIndices = Seq[Int]()
      while(matcher.find()) {
        matchIndices = matchIndices :+ matcher.start()
      }
      matchIndices = matchIndices map (m => charIndexToLine(program, m))
      if (matchIndices.nonEmpty) Some(PatternMatchResult(0, matchIndices)) else None
    }
  }

  private def matchRegexByLine(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Option[PatternMatchResult]] = {
    Future {
      val matcher = pattern.matcher("")
      var matchIndices = Seq[Int]()
      program.split("\n").zipWithIndex foreach {
        case (line, ind) => if (matcher.reset(line).find()) matchIndices = matchIndices :+ ind
      }
      if (matchIndices.nonEmpty) Some(PatternMatchResult(0, matchIndices)) else None
    }
  }

  private def charIndexToLine(str: String, index: Int) = {
    str.substring(0, index).count(_ == '\n')
  }
}

case class PatternMatchResult(programEntryId: Long, matchIndices: Seq[Int])

