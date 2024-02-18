package util

import dataCollection.ProcessingHelper.{doProgramPrintsMatch, filterSiliconArgs}
import dataCollection.customFrontends.ProgramSyntaxProperties
import dataCollection.{ComparableProgramPrint, FPNode, Fingerprinter, ProgramPrint}
import database.tools.PatternMatcher
import database.{DBConnection, DBQueryInterface, PGSlickTables}
import queryFrontend._
import upickle.default.{read, write}
import viper.silver.parser.{FastParser, PBinExp, PCall, PNode, PProgram}
import webAPI.JSONReadWriters._

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Files, Paths}
import java.sql.Timestamp
import java.time.{Instant, LocalDateTime}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source.fromFile
import scala.io.{BufferedSource, Codec, Source}
import scala.reflect.io.Directory
import scala.sys.process.Process
import scala.util.{Failure, Success}
import java.time.Duration.between

//number of methods, maybe store larger one

// PR for Silver
// PR For Carbon, Silicon

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
  //printDDL()
  //specificMatch()
  //getMatchPercentages()
  //fpAllPrograms()
  //findDupTrees()
  //regexDBPerformance()
  //getFeatures()
  //testFilter()

  def printDDL(): Unit = {
    print(PGSlickTables.getDDL)
  }

//  def testFilter(): Unit = {
//    val testArr = Array("--timeout", "10", "--z3Exe", "local.exe", "--printMethodCFGs", "--randomFlag", "--cvc5Exe")
//    val filteredCorrectly = filterSiliconArgs(testArr) sameElements Array("--randomFlag")
//    println(filteredCorrectly)
//  }

  def getFeatures(): Unit = {
    import database.DBExecContext._
    val features = DBQueryInterface.getFeaturesById(501)
    features.onComplete {
      case Success(value) => value foreach (println(_))
      case Failure(exception) => println(exception)
    }
    Await.result(features, Duration.Inf)
  }

  def readProgram(path: String): String = {
    val fBuffer: BufferedSource = scala.io.Source.fromFile(path)(decoder)
    val prog =
      try fBuffer.mkString
      finally fBuffer.close()
    prog
  }

  def processingPerformanceTest(): Unit = {
    val dbAndAPIProcess = Process(s"./run.sh").run
    Thread.sleep(2000) // startup
    Await.ready(DBQueryInterface.clearDB(), Duration.Inf)
    try {
      var programs: Seq[ProgramEntry] = Seq()

      for(i<- 0 to 499) {
        val prog = readProgram(testFolder + s"dataCollection/others/prog${i}.vpr")
        val us = ProgramEntry(
          0,
          Timestamp.valueOf(LocalDateTime.now()),
          prog,
          15,
          "Silicon",
          "Silicon",
          Array(),
          2000,
          true
        )
        programs = programs :+ us
      }

      val insertQ = DBQueryInterface.insertProgramEntries(programs)
      Await.ready(insertQ, Duration.Inf)

      for(i <- 0 to 5) {
        APIQueries.submitProgram(programs(i).program, "Silicon", Array(), "Silicon", true, 2000)
      }
      Thread.sleep(300000)
    } finally {
      dbAndAPIProcess.destroy()
    }
  }

  def getMatchPercentages(): Unit = {
    val fileDir = new Directory(new File("src/test/resources/SimilarityTest/Matching/Viper"))
    val subDirs = fileDir.dirs
    var min: Seq[Double] = Seq()
    var max: Seq[Double] = Seq()
    println("Matching / Viper")
    for (d <- subDirs) {
      val res = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", false)
      min = min :+ res._1
      max = max :+ res._2
    }
    var min_mean = min.sum / min.length
    var min_stdev = math.sqrt(((min.map(_ - min_mean)).map(v => v * v).sum) / (min.length - 1))
    var max_mean = max.sum / max.length
    var max_stdev = math.sqrt(((max.map(_ - max_mean)).map(v => v * v).sum) / (max.length - 1))
    println(s"min_mean: ${min_mean}")
    println(s"min_stdev: ${min_stdev}")
    println(s"max_mean: ${max_mean}")
    println(s"max_stdev: ${max_stdev}")
    println()

    val fileDir2 = new Directory(new File("src/test/resources/SimilarityTest/Matching/Frontends"))
    val subDirs2 = fileDir2.dirs
    min = Seq()
    max = Seq()
    println("Matching / Frontends")
    for (d <- subDirs2) {
      val res = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", true)
      min = min :+ res._1
      max = max :+ res._2
    }
    min_mean = min.sum / min.length
    min_stdev = math.sqrt(((min.map(_ - min_mean)).map(v => v * v).sum) / (min.length - 1))
    max_mean = max.sum / max.length
    max_stdev = math.sqrt(((max.map(_ - max_mean)).map(v => v * v).sum) / (max.length - 1))
    println(s"min_mean: ${min_mean}")
    println(s"min_stdev: ${min_stdev}")
    println(s"max_mean: ${max_mean}")
    println(s"max_stdev: ${max_stdev}")
    println()

    val fileDir3 = new Directory(new File("src/test/resources/SimilarityTest/NotMatching/Viper"))
    val subDirs3 = fileDir3.dirs
    println("NotMatching / Viper")
    min = Seq()
    max = Seq()
    for (d <- subDirs3) {
      val res = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", false)
      min = min :+ res._1
      max = max :+ res._2
    }
    min_mean = min.sum / min.length
    min_stdev = math.sqrt(((min.map(_ - min_mean)).map(v => v * v).sum) / (min.length - 1))
    max_mean = max.sum / max.length
    max_stdev = math.sqrt(((max.map(_ - max_mean)).map(v => v * v).sum) / (max.length - 1))
    println(s"min_mean: ${min_mean}")
    println(s"min_stdev: ${min_stdev}")
    println(s"max_mean: ${max_mean}")
    println(s"max_stdev: ${max_stdev}")
    println()

    val fileDir4 = new Directory(new File("src/test/resources/SimilarityTest/NotMatching/Frontends"))
    val subDirs4 = fileDir4.dirs
    println("NotMatching / Frontends")
    min = Seq()
    max = Seq()
    for (d <- subDirs4) {
      val res = Helper.doMatch(d.toString() + "/prog1.vpr", d.toString() + "/prog2.vpr", true)
      min = min :+ res._1
      max = max :+ res._2
    }
    min_mean = min.sum / min.length
    min_stdev = math.sqrt(((min.map(_ - min_mean)).map(v => v * v).sum) / (min.length - 1))
    max_mean = max.sum / max.length
    max_stdev = math.sqrt(((max.map(_ - max_mean)).map(v => v * v).sum) / (max.length - 1))
    println(s"min_mean: ${min_mean}")
    println(s"min_stdev: ${min_stdev}")
    println(s"max_mean: ${max_mean}")
    println(s"max_stdev: ${max_stdev}")
    println()

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
    var progs: Seq[ProgramEntry] = Seq()
    var finalProgs: Seq[ProgramEntry] = Seq()
    for (i <- 0 to 99) {
      val file = new File(s"src/test/resources/dataCollection/frontends/prog${i}.vpr")
      //val file = new File("src/test/resources/ProcessingTest/sample.vpr")
      val buffer = fromFile(file)
      val prog = {
        try buffer.mkString
        finally buffer.close()
      }
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
      progs = progs :+ programEntry
    }
    for (i <- 1 to 50) {
      finalProgs = finalProgs ++ progs
    }
    val insertQ = DBQueryInterface.insertProgramEntries(finalProgs)
    Await.ready(insertQ, Duration.Inf)
    var rtimes: Seq[Double] = Seq()
    var progtimes: Seq[Double] = Seq()
    for (i <- 1 to 10) {
      val regexStr = "\\{.*\\(.*\\).*\\}"
      val starttime = Instant.now()
      val res = PatternMatcher.matchRegexAgainstDatabase(regexStr)
      val runtime = between(starttime, Instant.now()).toNanos / 1000000.0
      rtimes = rtimes :+ runtime
      progtimes = progtimes :+ (runtime / finalProgs.length)
    }
    println(rtimes.sum.toDouble / rtimes.length.toDouble)
    println(progtimes.sum.toDouble / progtimes.length.toDouble)
    dbProcess.destroy()
  }

  def regexPerformance() = {
    var progs: Seq[String] = Seq()
    var finalprogs: Seq[String] = Seq()
    var runtimes: Seq[Double] = Seq()
    for(i <- 0 to 99){
      val file = new File(s"src/test/resources/dataCollection/frontends/prog${i}.vpr")
      //val file = new File("src/test/resources/ProcessingTest/sample.vpr")
      val buffer = fromFile(file)
      val prog = {
        try buffer.mkString
        finally buffer.close()
      }
      progs = progs :+ prog
    }
    for(i <- 1 to 50) {
      finalprogs = finalprogs ++ progs
    }
    var rtimes: Seq[Double] = Seq()
    var progtimes: Seq[Double] = Seq()
    for(i <- 1 to 10) {
      val regexStr = "\\{.*\\(.*\\).*\\}"
      val res = PatternMatcher.matchRegexOnPrograms(finalprogs, regexStr)
      //val runtime = between(res._2, Instant.now()).toNanos / 1000000.0
      //rtimes = rtimes :+ runtime
      //progtimes = progtimes :+ (runtime / finalprogs.length)
    }
    println(rtimes.sum.toDouble/rtimes.length.toDouble)
    println(progtimes.sum.toDouble/progtimes.length.toDouble)

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
    pn.subnodes foreach (printRec(_))
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

  def specificMatch(): Unit = {
    val path1 = testFolder + s"SimilarityTest/NotMatching/Viper/Type4Clone/prog1.vpr"
    val path2 = testFolder + s"SimilarityTest/NotMatching/Viper/Type4Clone/prog2.vpr"
    val sourcefile1: BufferedSource = fromFile(path1)
    val program1: PProgram =
      try fastParser.parse(sourcefile1.mkString, Paths.get(path1))
      finally sourcefile1.close()
    val sourcefile2: BufferedSource = fromFile(path2)
    val program2: PProgram =
      try fastParser.parse(sourcefile2.mkString, Paths.get(path2))
      finally sourcefile2.close()
    val pprint1 = new ComparableProgramPrint(Fingerprinter.fingerprintPProgram(program1))
    val pprint2 = new ComparableProgramPrint(Fingerprinter.fingerprintPProgram(program2))
    println(pprint1.matchTrees(pprint2))
    println(pprint2.matchTrees(pprint1))
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
    var progresults: Seq[ProgramPrint] = Seq()
    var runtimes: Seq[Double] = Seq()
    for (num <- Seq.range(0, 100)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"dataCollection/frontend_results/prog${num}pprint.json")
      val pprintJSON: String =
        try sourcefile.mkString
        finally sourcefile.close()
      val progres = read[ProgramPrint](pprintJSON)
      progresults = progresults :+ progres
    }
    for (num <- Seq.range(0, 100)) {
      val prog1             = progresults(num)
      for (num2 <- Seq.range(num + 1, 100)) {
        val prog2 = progresults(num2)
        val starttime = Instant.now()
        doProgramPrintsMatch(prog1, prog2, "Nagini")
        runtimes = runtimes :+ between(starttime, Instant.now()).toNanos / 1000000.0
      }
    }
    val w = new BufferedWriter(new FileWriter(testFolder + s"dataCollection/runtimes.txt"))
    w.write(runtimes.mkString(","))
    w.close()
  }

  def fpAllPrograms(): Unit = {
    val fp        = Fingerprinter
    var runtimes: Seq[Double] = Seq()
    var filelen: Seq[Int] = Seq()
    for (num <- Seq.range(0, 100)) {
      val sourcefile: BufferedSource = fromFile(testFolder + s"dataCollection/frontends/prog${num}.vpr")(decoder)
      val sourcestring: String =
        try sourcefile.mkString
        finally sourcefile.close()
      val prog   = fastParser.parse(sourcestring, Paths.get(testFolder + s"dataCollection/frontends/prog${num}.vpr"))
      val starttime: Instant = Instant.now()
      val pprint = fp.fingerprintPProgram(prog)
      val runtime: Double = between(starttime, Instant.now()).toNanos / 1000000.0
      runtimes = runtimes :+ runtime
      filelen = filelen :+ getLOC(sourcestring)
      val w      = new BufferedWriter(new FileWriter(testFolder + s"dataCollection/frontend_results/prog${num}pprint.json"))
      w.write(write(pprint))
      w.close()
    }
    val w      = new BufferedWriter(new FileWriter(testFolder + s"dataCollection/runtimes.txt"))
    w.write(runtimes.mkString(","))
    w.close()
    println(s"avg loc: ${filelen.sum.toDouble / filelen.length.toDouble}")
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

object Helper {

  val fp = new FastParser
  def doMatch(file1: String, file2: String, frontend: Boolean): (Double, Double) = {
    val sourcefile1: BufferedSource = fromFile(file1)
    val sourcefile2: BufferedSource = fromFile(file2)
    val sourcestring1: String =
      try sourcefile1.mkString
      finally sourcefile1.close()
    val sourcestring2: String =
      try sourcefile2.mkString
      finally sourcefile2.close()
    val prog1   = fp.parse(sourcestring1, Paths.get(file1))
    val prog2   = fp.parse(sourcestring2, Paths.get(file2))
    val pprint1 = new ComparableProgramPrint(Fingerprinter.fingerprintPProgram(prog1))
    val pprint2 = new ComparableProgramPrint(Fingerprinter.fingerprintPProgram(prog2))
    if(frontend) {
      return (math.min(pprint1.matchTrees(pprint2).methFunMatchP, pprint2.matchTrees(pprint1).methFunMatchP),
        math.max(pprint1.matchTrees(pprint2).methFunMatchP, pprint2.matchTrees(pprint1).methFunMatchP))
    } else {
      return (math.min(pprint1.matchTrees(pprint2).totalMatchP, pprint2.matchTrees(pprint1).totalMatchP),
        math.max(pprint1.matchTrees(pprint2).totalMatchP, pprint2.matchTrees(pprint1).totalMatchP))
    }

  }
}

