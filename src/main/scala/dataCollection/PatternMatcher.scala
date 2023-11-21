package dataCollection

import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.concurrent.{Await, ExecutionContext, Future}
import database.DBExecContext._

import scala.concurrent.duration.{Duration, SECONDS}

object PatternMatcher {

  def matchRegex(programs: Seq[String], regexStr: String): Seq[PatternMatchResult] = {
    val pattern: Pattern = Pattern.compile(regexStr)
    var futureResults = Seq[Future[Option[PatternMatchResult]]]()

    programs foreach {
      program => futureResults = futureResults :+ matchProgramToRegex(program, pattern)
    }

    val results = Await.result(Future.sequence(futureResults), Duration(100, SECONDS)).flatten
    results
  }

  def matchProgramToRegex(program: String, pattern: Pattern)(implicit ec: ExecutionContext): Future[Option[PatternMatchResult]] = {
    Future {
      val matcher = pattern.matcher("")
      var matches = Seq[Int]()
      program.split("\n").zipWithIndex foreach {
        case (line, ind) => if (matcher.reset(line).find()) matches = matches :+ ind
      }
      if (matches.nonEmpty) Some(PatternMatchResult(0, matches)) else None
    }
  }
}

case class PatternMatchResult(programEntryId: Long, matchIndices: Seq[Int]) {
  override def toString: String = {
    s"""Regex pattern was matched in programEntry $programEntryId at lines:
       | ${matchIndices.mkString(", ")}""".stripMargin
  }
}

