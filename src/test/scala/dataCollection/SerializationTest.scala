package dataCollection

import dataCollection.ProcessingHelper.abstractToVerError
import queryFrontend._
import org.scalatest.funsuite.AnyFunSuite
import viper.silver.ast.NoPosition
import viper.silver.parser.FastParser
import viper.silver.verifier.{ParseError, VerifierWarning}

import java.io.File
import scala.io.BufferedSource
import scala.io.Source.fromFile

class SerializationTest extends AnyFunSuite {

  import database.BinarySerializer._

  val fp = new FastParser

  test("Serialization of String Array") {
    val arr         = Array("sample.vpr", "--timeout", "10")
    val binArr      = serialize(arr)
    val restoredArr = deserialize[Array[String]](binArr)
    assert(arr.sameElements(restoredArr))
  }

  test("Serialization of ProgramPrint") {
    val file                       = new File("src/test/resources/SerializationTest/sample.vpr")
    val filebuffer: BufferedSource = fromFile(file)
    val sourcestring: String =
      try filebuffer.mkString
      finally filebuffer.close()
    val prog   = fp.parse(sourcestring, file.toPath)
    val pprint = Fingerprinter.fingerprintPProgram(prog)

    val binPrint      = serialize(pprint)
    val restoredPrint = deserialize[ProgramPrint](binPrint)
    assert(pprint == restoredPrint)
  }

  test("Serialization of Verification Errors") {
    val perr        = ParseError("sample error", NoPosition)
    val vwarn       = VerifierWarning("sample warning", NoPosition)
    val errArr      = Array(perr, vwarn) map abstractToVerError
    val binArr      = serialize(errArr)
    val restoredArr = deserialize[Array[VerError]](binArr)
    assert(errArr.sameElements(restoredArr))
  }

}
