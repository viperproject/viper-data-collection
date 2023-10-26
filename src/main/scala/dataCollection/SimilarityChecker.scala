package dataCollection

import upickle.default.{macroRW, read, write, ReadWriter => RW}
import viper.silicon.SiliconRunner.runMain
import viper.silicon.{Config, SiliconFrontend, SiliconRunner, SiliconRunnerInstance, reporting}
import viper.silver.ast.{NoPosition, Position}
import viper.silver.frontend.DefaultStates
import viper.silver.parser.{FastParser, PProgram}
import viper.silver.reporter.{ExceptionReport, InternalWarningMessage, StdIOReporter}
import viper.silver.verifier.{AbstractError, Success, VerificationResult}
import viper.silver.verifier.{AbstractVerificationError => SilAbstractVerificationError, Failure => SilFailure, Success => SilSuccess, TimeoutOccurred => SilTimeoutOccurred, VerificationResult => SilVerificationResult, Verifier => SilVerifier}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.io.{BufferedSource, Source}
import scala.io.Source.fromFile
import scala.math.Ordered.orderingToOrdered

object SimTestRunner extends App {
  private val testFolder = "/Users/simon/code/silicon/src/test/resources/dataCollection/results/"
  val starttime = System.currentTimeMillis()
  var progresults: Seq[ProgAnalysisResult] = Seq()
  for (num <- Seq.range(100,479)) {
    val sourcefile: BufferedSource = fromFile(testFolder + s"prog${num}_analysis.json")
    val pprintJSON: String = try sourcefile.mkString finally sourcefile.close()
    val progres = read(pprintJSON)(ProgAnalysisResult.rw)
    progresults = progresults :+ progres
  }
  var dupCount = 0
  var errCount = 0
  for (num <- Seq.range(100, 479)) {
    val prog1 = progresults(num-100)
    if(!(prog1.silRes.verRes == Some(VerRes(true, List())))) errCount +=1
    var matches: Seq[Int] = Seq()
    for(num2 <- Seq.range(num + 1, 479)) {
      val prog2 = progresults(num2-100)
      if (prog1 != prog2) {
        if(prog1.isSimilarTo(prog2) && prog2.isSimilarTo(prog1)) {
          matches = matches :+ num2
          //println(s"MATCH FOUND: ${prog1.progName} and ${prog2.progName}")
          //println(prog1.pprint.matchTrees(prog2.pprint))
        }
      }
    }
    println(s"Matches with ${num}: ${matches}")
    if (!(matches == List())) dupCount +=1
  }
  println(s"${dupCount} duplicates found")
  println(s"${errCount} errors found")
  println(s"Time: ${System.currentTimeMillis() - starttime}ms")
}

/** Represents the result of the [[ProgramAnalyser]] for later comparison*/
case class ProgAnalysisResult(progName: String, silRes: SiliconResult, pprint: ProgramPrint, flags: Seq[String]) {
  def isSimilarTo(other: ProgAnalysisResult): Boolean = {
    lazy val sameFlags = this.flags.toSet == other.flags.toSet
    lazy val similarResult = this.silRes.isSimilarTo(other.silRes, 1.5)
    lazy val matchResult = this.pprint.matchTrees(other.pprint)
    lazy val similarMethods = matchResult.methodMatchPercentage >= 80
    lazy val similarPreamble = matchResult.preambleMatchPercentage >= 80

    similarResult && sameFlags && similarPreamble && similarMethods
  }
}
object ProgAnalysisResult {
  implicit val rw: RW[ProgAnalysisResult] = macroRW
}

/** Result of running a program through the Silicon Verifier
 * @param verRes is the verification result, including any errors encountered
 * @param time is the runtime it took for the verifier to finish
 */
case class SiliconResult(verRes: Option[VerRes], time: Long) {
  def isSimilarTo(other: SiliconResult, timeEps: Double): Boolean = {
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
object SiliconResult {
  implicit val rw: RW[SiliconResult] = macroRW
}

/** A wrapper class for a [[VerificationResult]] to facilitate comparison and serialization*/
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

/** A wrapper class for an [[AbstractError]] to facilitate comparison and serialization and remove unneeded information*/
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

/** Object to analyse programs for later similarity comparison
 * @*/
object ProgramAnalyser {
  private val fastParser = new FastParser()

  /** Analyses a program and returns the results of the verifier and a fingerprint of its AST */
  def runProgAnalysis(args: Array[String]): ProgAnalysisResult = {
    val fileName = Paths.get(args(0)).getFileName.toString.split("\\.").head
    val silRes = getVerificationResults(args)
    val pprint = getProgramPrint(Paths.get(args(0)))
    ProgAnalysisResult(fileName, silRes, pprint, args.tail)
  }

  private def getVerificationResults(args: Array[String]): SiliconResult = {
    val runner = new SimSilRunnerInstance
    runner.runMain(args)
    SiliconResult(runner.getVerificationResult.map(v => VerRes.toVerRes(v)), runner.getTime)
  }

  private def getProgramPrint(p: Path): ProgramPrint = {
    val argString = Source.fromInputStream(Files.newInputStream(p)).mkString
    val parsedProgram = fastParser.parse(argString, p)
    Fingerprinter.fingerprintPProgram(parsedProgram)
  }
}

object AnalysisRunner {
  val testFolder = "/Users/simon/code/silicon/src/test/resources/dataCollection/results/"
  def main(args: Array[String]): Unit = {
    val fileName = Paths.get(args(0)).getFileName.toString.split("\\.").head
    val programData = ProgramAnalyser.runProgAnalysis(args)
    val resJSON = write(programData)
    val w = new BufferedWriter(new FileWriter(s"${testFolder}${fileName}_analysis.json"))
    w.write(resJSON)
    w.close()
  }
}


/** Silicon frontend implementation that doesn't exit the program once verification is done*/
class SimSilRunnerInstance extends SiliconFrontend(StdIOReporter()) {
  def runMain(args: Array[String]): Unit = {

    try {
      execute(ArraySeq.unsafeWrapArray(args))
    } catch {
      case exception: Exception
        if config == null || !config.asInstanceOf[Config].disableCatchingExceptions() =>
        if (config != null) config.assertVerified()
        reporting.exceptionToViperError(exception) match {
          case Right((cause, _)) =>
            reporter report ExceptionReport(exception)
            logger debug("An exception occurred:", cause)
          case Left(error: Error) =>
            error match {
              case _: NoClassDefFoundError =>
                reporter report InternalWarningMessage(reporting.noClassDefFoundErrorMessage)
                reporter report ExceptionReport(error)
                logger error(reporting.noClassDefFoundErrorMessage, error)
              case _ =>
            }

            throw error
        }
      case error: NoClassDefFoundError =>
        reporter report InternalWarningMessage(reporting.noClassDefFoundErrorMessage)
        reporter report ExceptionReport(error)
        logger error(reporting.noClassDefFoundErrorMessage, error)
    } finally {
      siliconInstance.stop()
    }
  }
}


