package util

import dataCollection.ProcessingHelper.doProgramPrintsMatch
import dataCollection.customFrontends.ProgramSyntaxProperties
import dataCollection.{ComparableProgramPrint, FPNode, Fingerprinter, ProgramPrint}
import database.tools.PatternMatcher
import database.{DBConnection, DBQueryInterface, PGSlickTables}
import queryFrontend._
import upickle.default.{read, write}
import viper.silver.parser.{FastParser, Nodes, PBinExp, PCall, PNode}
import webAPI.JSONReadWriters._

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Files, Paths}
import java.sql.Timestamp
import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source.fromFile
import scala.io.{BufferedSource, Codec, Source}
import scala.sys.process.Process
import scala.util.{Failure, Success}

//number of methods, maybe store larger one

// PR for Silver
// PR For Carbon, Silicon
// Add functionality to only submit, not evaluate programs
// remove filenames

object TestRunner extends App {
  private val testFolder = "/Users/simon/code/viper-data-collection/src/test/resources/"
  private val fastParser = new FastParser()
  private val decoder    = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)

  //showAST()
  //getPrograms()
  //println(PGSlickTables.getDDL)
  //findDups()
  //fpAllPrograms()
  //findDupTrees()
  //val startTime = System.currentTimeMillis()
  //naginiDups()
  //println("Took: " + ((System.currentTimeMillis() - startTime) / 1000))
  //specificResult(825, 834)
  //specificFingerPrint(2)
  //regexDBPerformance()
  //regexPerformance()
  //regexDBPerformance()
  //nodeTypeTest()
  //showAST()
  //findTerm("wildcard")
  //syntaxProps()
  //parseProgs
  //getPrograms()
  //getSiliconResults()
  //clearDatabase()
  printDDL()

  def printDDL(): Unit = {
    print(PGSlickTables.getDDL)
  }

  def clearDatabase(): Unit = {
    Await.ready(DBQueryInterface.clearDB(), Duration.Inf)
  }

  def getSiliconResults(): Unit = {
    import database.DBExecContext._
    val res = DBQueryInterface.getLatestSilResForEntry(20)
    res.onComplete {
      case Success(value) => value foreach println
      case Failure(exception) => println(exception)
    }
    Await.result(res, Duration.Inf)
  }

  def parseProgs: Unit = {
    val fp = Fingerprinter
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
      val sourcestring: String =
        try sourcefile.mkString
        finally sourcefile.close()
      val prog = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
      if (prog.errors != Seq.empty) println(s"program: $num ${prog.errors}")
    }
  }

  def syntaxProps(): Unit = {
    val file = new File("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val pprog =
      fastParser.parse(prog, Paths.get("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr"))
    val psp = new ProgramSyntaxProperties(prog, pprog)
    println(psp.getFeatures)
  }

  def nodeTypeTest() = {
    val file   = new File("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val pprog =
      fastParser.parse(prog, Paths.get("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr"))
    val startTime = System.currentTimeMillis()
    for (i <- 1 to 100) {
      val psp = new ProgramSyntaxProperties(prog, pprog)
      psp.hasSet
      psp.hasSeq
      psp.hasMagicWand
      psp.hasWildcardPerm
      psp.hasPerm
      psp.hasForPerm
      psp.hasTermination
      psp.hasRecursiveFunc
      psp.hasRecursivePred
      psp.hasMissingTrigger
      psp.mightHaveQP
    }
    println("Took: " + (((System.currentTimeMillis() - startTime).toDouble / 100) + " ms per iter"))
  }

  def regexDBPerformance() = {
    val dbProcess = Process("docker-compose up").run
    Await.ready(DBQueryInterface.clearDB(), Duration.Inf)
    val file = new File("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr")
    //val file = new File("src/test/resources/ProcessingTest/sample.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val programEntry: ProgramEntry = ProgramEntry(
      0,
      Timestamp.valueOf(LocalDateTime.now()),
      prog,
      1400,
      "Nagini",
      "Silicon",
      Array(),
      3000,
      true
    )
    val progs   = (for (i <- 1 to 5000) yield programEntry).toList
    val insertQ = DBQueryInterface.insertProgramEntries(progs)
    Await.ready(insertQ, Duration.Inf)
    val regexStr  = "\\{.*\\([^)]*\\).*\\}"
    val startTime = System.currentTimeMillis()
    val res       = PatternMatcher.matchRegexAgainstDatabase(regexStr)
    println("Took: " + ((System.currentTimeMillis() - startTime) + " ms"))
    println(res.map(_.matchIndices.length).sum + " lines were matched")
    dbProcess.destroy()
  }

  def regexPerformance() = {
    val file = new File("src/test/resources/SimilarityTest/Matching/Frontends/Subset/prog1.vpr")
    //val file = new File("src/test/resources/ProcessingTest/sample.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val progs     = (for (i <- 1 to 5000) yield prog).toList
    val regexStr  = "\\{.*\\([^)]*\\).*\\}"
    val startTime = System.currentTimeMillis()
    val res       = PatternMatcher.matchRegexOnPrograms(progs, regexStr)
    println("Took: " + ((System.currentTimeMillis() - startTime) + " ms"))
    println(res.map(_.matchIndices.length).sum + " lines were matched")
  }

  def getPrograms(): Unit = {
    import database.DBExecContext._
    val programs = DBQueryInterface.getAllProgramEntries()
    programs.onComplete {
      case Success(value)     => value foreach (v => println(v.frontend))
      case Failure(exception) => println(exception)
    }
    Await.result(programs, Duration.Inf)
  }

  def showAST(): Unit = {
    val file   = new File("src/test/resources/SimilarityTest/Matching/Frontends/SmallChangeInPython/prog2.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val progAST = fastParser.parse(prog, file.toPath)
    printRec(progAST)
  }

  def printRec(pn: PNode): Unit = {
    println(pn)
    Nodes.subnodes(pn) foreach (printRec(_))
  }

  def showFP(): Unit = {
    val file = new File("src/test/resources/SimilarityTest/Matching/Frontends/SmallChangeInRust/prog1.vpr")
    val buffer = fromFile(file)
    val prog =
      try buffer.mkString
      finally buffer.close()
    val progAST = fastParser.parse(prog, file.toPath)
    val pprint = Fingerprinter.fingerprintPProgram(progAST)
    println(countHash(pprint.methodTree, "3b147439f7226937"))
    //println(pprint.methodTree)
  }

  def countHash(n: FPNode, h: String): Int = {
    val childVals = (n.children map (c => countHash(c, h))).sum
    if (n.fp.hashVal == h) {
      childVals + 1
    } else {
      childVals
    }
  }

  def naginiDups(): Unit = {
    val folder                               = new File(testFolder + "SimilarityTest/Matching/Frontends/Subset/")
    var pprints: Seq[(String, ProgramPrint)] = Seq()
    for (f <- folder.listFiles()) {
      val sourcefile = fromFile(f)
      val text =
        try sourcefile.mkString
        finally sourcefile.close()
      val prog = fastParser.parse(text, f.toPath)
      pprints = pprints :+ (f.getName, Fingerprinter.fingerprintPProgram(prog))
      //println(Fingerprinter.fingerprintPProgram(prog))
    }
    var dups: Set[String] = Set()
    for ((name1, pprint1) <- pprints) {
      for ((name2, pprint2) <- pprints) {
        if (pprint1 != pprint2) {
          if (doProgramPrintsMatch(pprint1, pprint2, "Nagini")) {
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
    val pprintJSON1: String =
      try sourcefile1.mkString
      finally sourcefile1.close()
    val progres1                    = read[ProgramPrint](pprintJSON1)
    val sourcefile2: BufferedSource = fromFile(testFolder + s"results/prog${num2}pprint.json")
    val pprintJSON2: String =
      try sourcefile2.mkString
      finally sourcefile2.close()
    val progres2 = read[ProgramPrint](pprintJSON2)
    doProgramPrintsMatch(progres1, progres2, "Silicon")
  }

  def specificFingerPrint(num: Int): Unit = {
    val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
    val sourcestring: String =
      try sourcefile.mkString
      finally sourcefile.close()
    val prog   = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
    val pprint = Fingerprinter.fingerprintPProgram(prog)
    val w      = new BufferedWriter(new FileWriter(testFolder + s"results/prog${num}pprint.json"))
    w.write(write(pprint))
    w.close()
  }

  def findDupTrees(): Unit = {
    val starttime                      = System.currentTimeMillis()
    var progresults: Seq[ProgramPrint] = Seq()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"results/prog${num}pprint.json")
      val pprintJSON: String =
        try sourcefile.mkString
        finally sourcefile.close()
      val progres = read[ProgramPrint](pprintJSON)
      progresults = progresults :+ progres
    }
    var dupCount = 0
    for (num <- Seq.range(0, 901)) {
      val prog1             = progresults(num)
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
    val fp        = Fingerprinter
    val starttime = System.currentTimeMillis()
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"others/prog${num}.vpr")(decoder)
      val sourcestring: String =
        try sourcefile.mkString
        finally sourcefile.close()
      val prog   = fastParser.parse(sourcestring, Paths.get(testFolder + s"others/prog${num}.vpr"))
      val pprint = fp.fingerprintPProgram(prog)
      val w      = new BufferedWriter(new FileWriter(testFolder + s"results/prog${num}pprint.json"))
      w.write(write(pprint))
      w.close()
    }
    println(s"Time: ${System.currentTimeMillis() - starttime}")
  }

  def findTerm(term: String): Unit = {
    for (num <- Seq.range(0, 901)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"prog${num}.vpr")(decoder)
      val sourcestring: String =
        try sourcefile.mkString
        finally sourcefile.close()
      if (sourcestring.contains(term)) println(num)
    }
  }

  private def basicFingerprints(): Unit = {
    val fp = Fingerprinter

    val file1   = Paths.get(testFolder + "test.vpr")
    val string1 = Source.fromInputStream(Files.newInputStream(file1)).mkString

    val file2   = Paths.get(testFolder + "test2.vpr")
    val string2 = Source.fromInputStream(Files.newInputStream(file2)).mkString

    val file3   = Paths.get(testFolder + "test3.vpr")
    val string3 = Source.fromInputStream(Files.newInputStream(file3)).mkString

    val file4   = Paths.get(testFolder + "test4.vpr")
    val string4 = Source.fromInputStream(Files.newInputStream(file4)).mkString

    val pp1       = fastParser.parse(string1, file1)
    val pp1_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp1))

    val pp2       = fastParser.parse(string2, file2)
    val pp2_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp2))

    val pp3       = fastParser.parse(string3, file3)
    val pp3_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp3))

    val pp4       = fastParser.parse(string4, file4)
    val pp4_print = new ComparableProgramPrint(fp.fingerprintPProgram(pp4))

    // only variable names, whitespaces changed: expected full match
    println(pp1_print.matchTrees(pp2_print))
    // reordered some statements, high match expected
    println(pp1_print.matchTrees(pp3_print))
    // completely different program, low match expected -> remove low weight nodes
    println(pp1_print.matchTrees(pp4_print))
  }

}
