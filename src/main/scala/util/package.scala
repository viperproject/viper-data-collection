import database.BinarySerializer.{deserialize, serialize}

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, FileWriter}
import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.StandardOpenOption
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.reflect.io.Directory

/** Traits, classes and functions that don't necessitate their own file */
package object util {

  /** Tries to lock the global.lock file and keeps spinning until it gets the lock. To be used when program should eventually be executed
    *
    * @throws GlobalLockException if any exception occured
    */
  def getGlobalLockSpinning(sleepTime: Int = 100): FileLock = {
    val file = new File("src/main/resources/global.lock")
    try {
      val fc   = FileChannel.open(file.toPath, StandardOpenOption.WRITE)
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

  /** Tries to lock the global.lock file
    *
    * @returns the FileLock, should eventually be released again
    * @throws GlobalLockException if any exception occured or lock is held by someone else
    */
  def getGlobalLock(): FileLock = {
    val file = new File("src/main/resources/global.lock")
    try {
      val fc   = FileChannel.open(file.toPath, StandardOpenOption.WRITE)
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

  def loadSerializedObject[T <: Serializable](file: String): T = {
    val fileReader = new BufferedInputStream(new FileInputStream(file))
    val obj =
      try {
        deserialize[T](fileReader.readAllBytes())
      } finally {
        fileReader.close()
      }
    obj
  }

  def storeObjectSerialized[T <: Serializable](t: T, file: String): Unit = {
    val fileWriter = new BufferedOutputStream(new FileOutputStream(file))
    try {
      fileWriter.write(serialize[T](t))
    } finally {
      fileWriter.close()
    }
  }

  def createTempDir(dirName: String): Unit = {
    val dir = new File(s"tmp/$dirName")
    if (!dir.exists()) {
      dir.mkdir()
    }
  }

  def removeTempDir(dirName: String): Unit = {
    val dir = new Directory(new File(s"tmp/$dirName"))
    if (dir.exists) {
      dir.deleteRecursively()
    }
  }

  /** Verifiers and Parsers need a local file that contains the program, this function creates such a temporary file and returns the path */
  def createTempProgramFile(id: Long, program: String): String = {
    val fName          = s"./tmp/$id.vpr"
    val fw: FileWriter = new FileWriter(new File(fName))
    fw.write(program)
    fw.close()
    fName
  }

  /** Removes the temporary program file */
  def removeTempProgramFile(fName: String): Unit = {
    val f = new File(fName)
    f.delete()
  }

}
