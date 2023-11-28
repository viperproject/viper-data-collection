package dataCollection

import dataCollection.customFrontends.ProgramSyntaxProperties
import org.scalatest.funsuite.AnyFunSuite
import viper.silver.parser.{ FastParser, PProgram }

import java.io.File
import java.nio.file.Paths
import scala.io.Source.fromFile

class FeatureTest extends AnyFunSuite {
  val fastParser     = new FastParser
  val resourceFolder = "./src/test/resources/FeatureTest/"

  test("hasSet") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "set.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasSet)
  }
  test("hasSeq") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "seq.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasSeq)
  }
  test("hasMagicWand") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "magicwand.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasMagicWand)
  }
  test("hasWildcardPerm") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "wildcard.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasWildcardPerm)
  }
  test("hasPerm") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "perm.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasPerm)
  }
  test("hasForPerm") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "forperm.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasForPerm)
  }
  test("hasTermination") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "termination.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasTermination)
  }
  test("hasRecursivePred") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "recursivepred.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasRecursivePred)
  }
  test("hasRecursiveFunc") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "recursivefunc.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasRecursiveFunc)
  }
  test("hasMissingTrigger") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "missingtrigger.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.hasMissingTrigger)
  }
  test("mightHaveQP") {
    val (prog, pprog) = loadProgramAndParse(resourceFolder + "qp.vpr")
    val sp            = new ProgramSyntaxProperties(prog, pprog)
    assert(sp.mightHaveQP)
  }

  def loadProgramAndParse(path: String): (String, PProgram) = {
    val file   = new File(path)
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val pprog = fastParser.parse(prog, Paths.get(path))
    (prog, pprog)
  }

}
