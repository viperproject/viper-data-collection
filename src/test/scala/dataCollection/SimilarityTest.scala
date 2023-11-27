package dataCollection

import dataCollection.ProcessingHelper.doProgramPrintsMatch
import org.scalatest.funsuite.AnyFunSuite
import viper.silver.parser.FastParser

import java.io.File
import java.nio.file.Paths
import scala.io.BufferedSource
import scala.io.Source.fromFile
import scala.reflect.io.Directory

class SimilarityTest extends AnyFunSuite{
  import Helper._

  test("Viper programs that should match") {
    val fileDir = new Directory(new File("src/test/resources/SimilarityTest/Matching/Viper"))
    val subDirs = fileDir.dirs
    for (d <- subDirs) {
      val doMatch = Helper.doMatch(d.toString()+"/prog1.vpr", d.toString()+"/prog2.vpr", "Silicon")
      assert(doMatch)
    }

  }
  test("Frontend programs that should match") {
    val fileDir = new Directory(new File("src/test/resources/SimilarityTest/Matching/Frontends"))
    val subDirs = fileDir.dirs
    for (d <- subDirs) {
      val doMatch = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", "Generic")
      assert(doMatch)
    }
  }
  test("Viper programs that should not match"){
    val fileDir = new Directory(new File("src/test/resources/SimilarityTest/NotMatching/Viper"))
    val subDirs = fileDir.dirs
    for (d <- subDirs) {
      val doMatch = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", "Silicon")
      assert(!doMatch)
    }
  }
  test("Frontend programs that should not match"){
    val fileDir = new Directory(new File("src/test/resources/SimilarityTest/NotMatching/Frontends"))
    val subDirs = fileDir.dirs
    for (d <- subDirs) {
      val doMatch = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", "Generic")
      assert(!doMatch)
    }
  }

}

object Helper {

  val fp = new FastParser
  def doMatch(file1: String, file2: String, frontend: String): Boolean = {
    val sourcefile1: BufferedSource = fromFile(file1)
    val sourcefile2: BufferedSource = fromFile(file2)
    val sourcestring1: String = try sourcefile1.mkString finally sourcefile1.close()
    val sourcestring2: String = try sourcefile2.mkString finally sourcefile2.close()
    val prog1 = fp.parse(sourcestring1, Paths.get(file1))
    val prog2 = fp.parse(sourcestring2, Paths.get(file2))
    val pprint1 = Fingerprinter.fingerprintPProgram(prog1)
    val pprint2 = Fingerprinter.fingerprintPProgram(prog2)
    doProgramPrintsMatch(pprint1, pprint2, frontend)
  }
}
