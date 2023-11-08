import dataCollection.{Fingerprinter, ProgramPrint, ProgramSimilarityInfo}
import database.{DBQueryInterface, MySQLSlickTables}
import viper.silver.parser.FastParser
import upickle.default.{macroRW, read, write, ReadWriter => RW}
import slick.jdbc.MySQLProfile.api._

import java.io.File
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

  //getPrograms()
  println(MySQLSlickTables.getDDL)
  //findDupTrees()
  //naginiDups()

  def getPrograms(): Unit = {
    import database.ExecContext._
    val programs = DBQueryInterface.getAllProgramEntries()
    programs.onComplete {
      case Success(value) => println(value)
      case Failure(exception) => println(exception)
    }
    Await.result(programs, Duration.Inf)
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
          val matchres1 = pprint1.matchTrees(pprint2)
          val matchres2 = pprint2.matchTrees(pprint1)
          if (matchres1.funAndMethMatchPercentage >= 80 && matchres2.funAndMethMatchPercentage >= 80){
              println(matchres1)
              println(s"MATCH FOUND: ${name1}, ${name2}")
              dups = dups.union(Set(name1, name2))
          }
        }
      }
    }
    println(dups)
    println(dups.size)
  }

  def findDupTrees(): Unit = {
    val starttime = System.currentTimeMillis()
    var progresults: Seq[ProgramPrint] = Seq()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"results/prog${num}pprint.json")
      val pprintJSON: String = try sourcefile.mkString finally sourcefile.close()
      val progres = read(pprintJSON)(ProgramPrint.rw)
      progresults = progresults :+ progres
    }
    var dupCount = 0
    for (num <- Seq.range(0, 901)) {
      val prog1 = progresults(num)
      var matches: Seq[Int] = Seq()
      for (num2 <- Seq.range(num + 1, 901)) {
        val prog2 = progresults(num2)
        val matchres1 = prog1.matchTrees(prog2)
        val matchres2 = prog2.matchTrees(prog1)
        if (matchres1.funAndMethMatchPercentage >= 90 && matchres2.funAndMethMatchPercentage >= 90) {
            matches = matches :+ num2
        }
      }
      println(s"Matches with ${num}: ${matches}")
      if (!(matches == List())) dupCount += 1
    }
    println(s"${dupCount} duplicates found")
    println(s"Time: ${System.currentTimeMillis() - starttime}ms")
  }
  def findDups(): Unit = {
    val starttime = System.currentTimeMillis()
    var progresults: Seq[ProgramSimilarityInfo] = Seq()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"results/prog${num}_analysis.json")
      val pprintJSON: String = try sourcefile.mkString finally sourcefile.close()
      val progres = read(pprintJSON)(ProgramSimilarityInfo.rw)
      progresults = progresults :+ progres
    }
    var dupCount = 0
    for (num <- Seq.range(0, 901)) {
      val prog1 = progresults(num)
      var matches: Seq[Int] = Seq()
      for (num2 <- Seq.range(num + 1, 901)) {
        val prog2 = progresults(num2)
        if (prog1 != prog2) {
          if (prog1.isSimilarTo(prog2) && prog2.isSimilarTo(prog1)) {
            matches = matches :+ num2
          }
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
    var pprints: Seq[ProgramPrint] = Seq()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
      val sourcestring: String = try sourcefile.mkString finally sourcefile.close()
      val prog = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
      val pprint = Fingerprinter.fingerprintPProgram(prog)
      pprint.store(testFolder + s"results/prog${num}pprint.json")
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
    val pp1_print = fp.fingerprintPProgram(pp1)

    val pp2 = fastParser.parse(string2, file2)
    val pp2_print = fp.fingerprintPProgram(pp2)

    val pp3 = fastParser.parse(string3, file3)
    val pp3_print = fp.fingerprintPProgram(pp3)

    val pp4 = fastParser.parse(string4, file4)
    val pp4_print = fp.fingerprintPProgram(pp4)

    pp1_print.store(testFolder + "prog1.json")
    val pprint1_clone = ProgramPrint.load(testFolder + "prog1.json")
    println(pprint1_clone)

    // only variable names, whitespaces changed: expected full match
    println(pp1_print.matchTrees(pp2_print))
    // reordered some statements, high match expected
    println(pp1_print.matchTrees(pp3_print))
    // completely different program, low match expected -> remove low weight nodes
    println(pp1_print.matchTrees(pp4_print))
  }

}
