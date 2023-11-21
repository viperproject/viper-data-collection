import dataCollection.ProcessingHelper.doProgramPrintsMatch
import dataCollection.{ComparableProgramPrint, Fingerprinter, ProgramPrint}
import database.{DBQueryInterface, PGSlickTables}
import viper.silver.parser.FastParser
import upickle.default.{macroRW, read, write, ReadWriter => RW}
import slick.jdbc.MySQLProfile.api._
import webAPI.JSONReadWriters._

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Files, Paths}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source.fromFile
import scala.io.{BufferedSource, Codec}
import scala.io.Source
import scala.util.{Failure, Success}

//number of methods, maybe store larger one

object TestRunner extends App {
  private val testFolder = "/Users/simon/code/viper-data-collection/src/test/resources/dataCollection/"
  private val fastParser = new FastParser()
  private val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)

  //showAST()
  //getPrograms()
  println(PGSlickTables.getDDL)
  //findDups()
  //fpAllPrograms()
  //findDupTrees()
  //val startTime = System.currentTimeMillis()
  //naginiDups()
  //println("Took: " + ((System.currentTimeMillis() - startTime) / 1000))
  //specificResult(825, 834)
  //specificFingerPrint(2)


  def getPrograms(): Unit = {
    import database.DBExecContext._
    val programs = DBQueryInterface.getAllProgramEntries()
    programs.onComplete {
      case Success(value) => println(value)
      case Failure(exception) => println(exception)
    }
    Await.result(programs, Duration.Inf)
  }

  def showAST(): Unit = {
    val file = new File("src/test/resources/SimilarityTest/Matching/Viper/AndPredicates/prog1.vpr")
    val buffer = fromFile(file)
    val prog = try buffer.mkString finally buffer.close()
    val progAST = fastParser.parse(prog, file.toPath)
    println(progAST)
  }

  def naginiDups(): Unit = {
    val folder = new File(testFolder + "nagini_full/")
    var pprints: Seq[(String, ProgramPrint)] = Seq()
    for (f <- folder.listFiles()) {
      val sourcefile = fromFile(f)
      val text = try sourcefile.mkString finally sourcefile.close()
      val prog = fastParser.parse(text, f.toPath)
      pprints = pprints :+ (f.getName, Fingerprinter.fingerprintPProgram(prog))
    }
    var dups: Set[String] = Set()
    for ((name1, pprint1) <- pprints) {
      for ((name2, pprint2) <- pprints) {
        if (pprint1 != pprint2) {
          if(doProgramPrintsMatch(pprint1, pprint2, "Nagini")) {
              println(s"MATCH FOUND: ${name1}, ${name2}")
            dups = dups.union(Set(name1, name2))
          }
        }
      }
    }
    println(dups)
    println(dups.size)
  }

  def specificResult(num1: Int, num2: Int): Unit = {
    val sourcefile1: BufferedSource = fromFile(testFolder + s"results/prog${num1}pprint.json")
    val pprintJSON1: String = try sourcefile1.mkString finally sourcefile1.close()
    val progres1 = read[ProgramPrint](pprintJSON1)
    val sourcefile2: BufferedSource = fromFile(testFolder + s"results/prog${num2}pprint.json")
    val pprintJSON2: String = try sourcefile2.mkString finally sourcefile2.close()
    val progres2 = read[ProgramPrint](pprintJSON2)
    doProgramPrintsMatch(progres1, progres2, "Silicon")
  }

  def specificFingerPrint(num: Int): Unit = {
    val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
    val sourcestring: String = try sourcefile.mkString finally sourcefile.close()
    val prog = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
    val pprint = Fingerprinter.fingerprintPProgram(prog)
    val w = new BufferedWriter(new FileWriter(testFolder + s"results/prog${num}pprint.json"))
    w.write(write(pprint))
    w.close()
  }

  def findDupTrees(): Unit = {
    val starttime = System.currentTimeMillis()
    var progresults: Seq[ProgramPrint] = Seq()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"results/prog${num}pprint.json")
      val pprintJSON: String = try sourcefile.mkString finally sourcefile.close()
      val progres = read[ProgramPrint](pprintJSON)
      progresults = progresults :+ progres
    }
    var dupCount = 0
    for (num <- Seq.range(0, 901)) {
      val prog1 = progresults(num)
      var matches: Seq[Int] = Seq()
      for (num2 <- Seq.range(num + 1, 901)) {
        val prog2 = progresults(num2)
        if (doProgramPrintsMatch(prog1, prog2, "Silicon")) {
              matches = matches :+ num2
        }
      }
      println(s"Matches with ${num}: ${matches}")
      if (!(matches == List())) dupCount += 1
    }
    println(s"${dupCount} duplicates found")
    println(s"Time: ${System.currentTimeMillis() - starttime}ms")
  }

  def fpAllPrograms(): Unit = {
    val fp = Fingerprinter
    val starttime = System.currentTimeMillis()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
      val sourcestring: String = try sourcefile.mkString finally sourcefile.close()
      val prog = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
      val pprint = fp.fingerprintPProgram(prog)
      val w = new BufferedWriter(new FileWriter(testFolder + s"results/prog${num}pprint.json"))
      w.write(write(pprint))
      w.close()
    }
    println(s"Time: ${System.currentTimeMillis() - starttime}")
  }

  private def basicFingerprints(): Unit = {
    val fp = Fingerprinter

    val file1 = Paths.get(testFolder + "test.vpr")
    val string1 = Source.fromInputStream(Files.newInputStream(file1)).mkString

    val file2 = Paths.get(testFolder + "test2.vpr")
    val string2 = Source.fromInputStream(Files.newInputStream(file2)).mkString

    val file3 = Paths.get(testFolder + "test3.vpr")
    val string3 = Source.fromInputStream(Files.newInputStream(file3)).mkString

    val file4 = Paths.get(testFolder + "test4.vpr")
    val string4 = Source.fromInputStream(Files.newInputStream(file4)).mkString

    val pp1 = fastParser.parse(string1, file1)
    val pp1_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp1))

    val pp2 = fastParser.parse(string2, file2)
    val pp2_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp2))

    val pp3 = fastParser.parse(string3, file3)
    val pp3_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp3))

    val pp4 = fastParser.parse(string4, file4)
    val pp4_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp4))

    // only variable names, whitespaces changed: expected full match
    println(pp1_print.matchTrees(pp2_print))
    // reordered some statements, high match expected
    println(pp1_print.matchTrees(pp3_print))
    // completely different program, low match expected -> remove low weight nodes
    println(pp1_print.matchTrees(pp4_print))
  }

}
