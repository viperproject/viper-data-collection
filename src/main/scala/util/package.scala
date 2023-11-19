import scala.concurrent.duration.{Duration, MILLISECONDS}

/** Classes and Traits that don't need their own file */
package object util {
  val DEFAULT_DB_TIMEOUT = Duration(10000, MILLISECONDS)

  trait Similarity[T] {

    def isSimilarTo(other: T): Boolean

  }

  case class NothingToDoException() extends Exception
  case class StageIncompleteException() extends Exception

  def getLOC(program: String): Int = {
    program.split("\n").length
  }

}
