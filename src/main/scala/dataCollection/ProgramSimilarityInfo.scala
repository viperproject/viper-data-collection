package dataCollection

import upickle.default.{macroRW, read, write, ReadWriter => RW}
import viper.silicon.SiliconFrontend
import viper.carbon.CarbonFrontend
import viper.silver.logger.{ViperLogger, ViperStdOutLogger}
import viper.silver.parser.FastParser
import viper.silver.reporter.{NoopReporter, StdIOReporter}
import viper.silver.verifier.{AbstractError, VerificationResult}
import viper.silver.verifier.{Failure => SilFailure, Success => SilSuccess}

import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.io.{BufferedSource, Codec}
import scala.io.Source.fromFile
import scala.language.postfixOps

object SimTestRunner extends App {
  private val testFolder = "/Users/simon/code/viper-data-collection/src/test/resources/dataCollection/"
  AnalysisRunner.main(Array(testFolder + "test.vpr", "--ideModeAdvanced"))
  /*val starttime = System.currentTimeMillis()
  var progresults: Seq[ProgAnalysisResult] = Seq()
  for (num <- Seq.range(0, 901)) {
    val sourcefile: BufferedSource = fromFile(testFolder + s"prog${num}_analysis.json")
    val pprintJSON: String = try sourcefile.mkString finally sourcefile.close()
    val progres = read(pprintJSON)(ProgAnalysisResult.rw)
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
  println(s"Time: ${System.currentTimeMillis() - starttime}ms")*/
}

/** Represents the result of the [[ProgramInfoAnalyser]] for later comparison
 *
 * @param siliconRes contains the [[VerifierResult]] and runtime of verifying the program through Silicon
 * @param carbonRes  contains the [[VerifierResult]] and runtime of verifying the program through Carbon
 * @param pprint     is the [[ProgramPrint]] of the program's AST
 * @param flags      are the arguments used to run the program */
case class ProgramSimilarityInfo(siliconRes: VerifierResult, carbonRes: VerifierResult, pprint: ProgramPrint, flags: Seq[String]) {
  def isSimilarTo(other: ProgramSimilarityInfo): Boolean = {
    lazy val sameFlags = this.flags.toSet == other.flags.toSet
    lazy val similarSilResult = this.siliconRes.isSimilarTo(other.siliconRes, 1.5)
    lazy val similarCarbonResult = this.carbonRes.isSimilarTo(other.carbonRes, 1.5)
    lazy val matchResult = this.pprint.matchTrees(other.pprint)
    lazy val similarMethods = matchResult.methodMatchPercentage >= 80
    lazy val similarPreamble = matchResult.preambleMatchPercentage >= 80

    sameFlags && similarSilResult && similarCarbonResult && similarPreamble && similarMethods
  }
}

object ProgramSimilarityInfo {
  implicit val rw: RW[ProgramSimilarityInfo] = macroRW
}

/** Result of running a program through some Viper Verifier
 *
 * @param verRes is the verification result, including any errors encountered
 * @param time   is the runtime it took for the verifier to finish
 */
case class VerifierResult(verRes: Option[VerRes], time: Long) {
  def isSimilarTo(other: VerifierResult, timeEps: Double): Boolean = {
    lazy val sameRes: Boolean = this.verRes match {
      case Some(res) => other.verRes match {
        case Some(otherRes) => res.isSimilarTo(otherRes)
        case None => false
      }
      case None => other.verRes == None
    }
    lazy val similarTime = (this.time <= other.time * timeEps && this.time >= other.time / timeEps)
    similarTime && sameRes
  }
}

object VerifierResult {
  implicit val rw: RW[VerifierResult] = macroRW
}

/** A wrapper class for a [[VerificationResult]] to facilitate comparison and serialization
 *
 * @param success whether the program verified successfully or failed
 * @param errors  if verification failed, contains [[VerError]]s describing the failure causes */
case class VerRes(success: Boolean, errors: Seq[VerError]) {
  def isSimilarTo(other: VerRes): Boolean = {
    success match {
      case true => other.success
      case false => other.errors.toSet == errors.toSet
    }
  }
}

object VerRes {
  implicit val rw: RW[VerRes] = macroRW

  def toVerRes(vr: VerificationResult): VerRes = vr match {
    case SilSuccess => VerRes(true, Seq.empty)
    case SilFailure(errors) => VerRes(false, errors map VerError.toError)
  }

}

/** A wrapper class for an [[AbstractError]] to facilitate comparison and serialization and remove unneeded information
 * Comparison is only done through [[id]], since [[message]]s are too specific to a given program
 *
 * @param id      the original error ID
 * @param message describes the error in full */
case class VerError(id: String, message: String) {
  override def equals(obj: Any): Boolean = obj match {
    case that: VerError => this.id == that.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode
}

object VerError {
  implicit val rw: RW[VerError] = macroRW

  def toError(ae: AbstractError): VerError = {
    VerError(ae.fullId, ae.readableMessage)
  }
}

object SimilarityChecker {
}

/** Object to analyse programs for later similarity comparison */
object ProgramInfoAnalyser {
  private val fastParser = new FastParser()
  val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)

  /** Analyses a program and returns the results of the verifier and a fingerprint of its AST */
  def runProgAnalysis(args: Array[String]): ProgramSimilarityInfo = {
    val silRes = getSilVerifierResults(args)
    val carbonRes = getCarbonVerifierResults(args)
    val pprint = getProgramPrint(Paths.get(args(0)))
    ProgramSimilarityInfo(silRes, carbonRes, pprint, args.tail)
  }

  private def getSilVerifierResults(args: Array[String]): VerifierResult = {
    val runner = new SiliconFEInstance
    runner.runMain(args)
    VerifierResult(runner.getVerificationResult.map(v => VerRes.toVerRes(v)), runner.getTime)
  }

  private def getCarbonVerifierResults(args: Array[String]): VerifierResult = {
    val runner = new CarbonFEInstance
    runner.main(args)
    VerifierResult(runner.getVerificationResult.map(v => VerRes.toVerRes(v)), runner.getTime)
  }

  private def getProgramPrint(p: Path): ProgramPrint = {
    val sourcefile: BufferedSource = fromFile(p.toString)(decoder)
    val argString: String = try sourcefile.mkString finally sourcefile.close()
    val parsedProgram = fastParser.parse(argString, p)
    Fingerprinter.fingerprintPProgram(parsedProgram)
  }
}

object AnalysisRunner {
  val testFolder = "/Users/simon/code/viper-data-collection/src/test/resources/dataCollection/results/"

  def main(args: Array[String]): Unit = {
    val fileName = Paths.get(args(0)).getFileName.toString.split("\\.").head
    val programData = ProgramInfoAnalyser.runProgAnalysis(args)
    val resJSON = write(programData)
    val w = new BufferedWriter(new FileWriter(s"${testFolder}${fileName}_analysis.json"))
    w.write(resJSON)
    w.close()
  }
}

class CarbonFEInstance extends CarbonFrontend(StdIOReporter("carbon_reporter"), ViperStdOutLogger("Carbon", "INFO").get) {
  def main(args: Array[String]): Unit = {
    runWithTimeout(5)(try {
      execute(Seq(args(0)))
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    })
  }

  def runWithTimeout[T]
  (timeout: Long)
  (f: => T)
  : Option[T] = {
    try {
      Some(Await.result(Future(f), timeout.seconds))
    } catch {
      case e: TimeoutException => None
    }
  }

}


/** Silicon frontend implementation that doesn't exit the program once verification is done */
class SiliconFEInstance extends SiliconFrontend(StdIOReporter(), logger = ViperLogger("vlogger", "out.txt", level = "ALL").get) {
  def runMain(args: Array[String]): Unit = {
    try {
      execute(ArraySeq.unsafeWrapArray(args))
    } catch {
      case e: Exception => println(s"encountered: ${e}")
    }
    finally {
      siliconInstance.stop()
    }
  }

  override def runAllPhases(): Unit = {
    phases.foreach(ph => {
      logger.trace(s"Frontend: running phase ${ph.name}, ${getTime}")
      ph.f()
    })
  }
}



