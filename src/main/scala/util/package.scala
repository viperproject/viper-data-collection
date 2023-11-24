import java.io.File
import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.StandardOpenOption
import scala.concurrent.duration.{Duration, MILLISECONDS}

/** Classes and Traits that don't need their own file */
package object util {


  trait Similarity[T] {

    def isSimilarTo(other: T): Boolean

  }

  def getGlobalLockSpinning(sleepTime: Int = 100): FileLock = {
    val file = new File("src/main/resources/global.lock")
    try {
      val fc = FileChannel.open(file.toPath, StandardOpenOption.WRITE)
      var lock = fc.tryLock()
      while (lock == null) {
        Thread.sleep(sleepTime)
        lock = fc.tryLock()
      }
      lock
    } catch {
      case e: Exception =>
        throw new GlobalLockException()
    }
  }

  def getGlobalLock(): FileLock = {
    val file = new File("src/main/resources/global.lock")
    try {
      val fc = FileChannel.open(file.toPath, StandardOpenOption.WRITE)
      val lock = fc.tryLock()
      if (lock == null) throw GlobalLockException()
      lock
    } catch {
      case e: Exception =>
        throw new GlobalLockException()
    }
  }

  case class NothingToDoException() extends Exception

  case class StageIncompleteException() extends Exception

  case class GlobalLockException() extends Exception

  def getLOC(program: String): Int = {
    program.split("\n").length
  }

}
