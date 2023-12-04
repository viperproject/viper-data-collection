package dataCollection.customFrontends

import database.tools.PatternMatcher
import viper.silver.parser.{FastParser, Nodes, PAccPred, PCall, PCurPerm, PEpsilon, PFullPerm, PNoPerm, PNode, PProgram, PQuantifier, PWildcard}
import viper.silver.reporter.{BenchmarkingPhase, Message, Reporter}
import viper.silver.verifier.{AbstractError, TypecheckerError}

import java.io.File
import java.nio.file.Paths
import java.util.regex.Pattern
import scala.io.Source.fromFile

trait FeatureGenerator {
  var syntaxProps: ProgramSyntaxProperties
  var hasRun: Boolean

  def instantiateSyntaxProps(programPath: String): Unit = {
    val fastParser = new FastParser
    val file       = new File(programPath)
    val buffer     = fromFile(file)
    val program =
      try buffer.mkString
      finally buffer.close()
    val pProgram = fastParser.parse(program, Paths.get(programPath))
    syntaxProps = new ProgramSyntaxProperties(program, pProgram)
  }

  def errors: Seq[AbstractError]

  def doesTypeCheck: Boolean = !errors.exists(e => e.isInstanceOf[TypecheckerError])

  /** Any VerifierFeatures returned by this function will be inserted into the database */
  def getFeatures: Seq[VerifierFeature] = if (hasRun)
    syntaxProps.getFeatures :+ VerifierFeature("typeCheckSuccess", doesTypeCheck.toString)
  else Seq()
}

/** Trait to extend CollectionSiliconFrontend with to generate features while verifying */
trait SilFeatureGenerator extends FeatureGenerator {

  def getBenchmarkResults: Seq[(String, Long)]

  private def benchmarkResToVF(res: Seq[(String, Long)]): Seq[VerifierFeature] =
    res map { case (phase, time) =>
      VerifierFeature(s"BenchmarkingPhase $phase", time.toString)
    }
  override def getFeatures: Seq[VerifierFeature] = {
    if (hasRun) super.getFeatures ++ benchmarkResToVF(getBenchmarkResults)
    else Seq()
  }
}

/** Trait to extend CollectionCarbonFrontend with to generate features while verifying */
trait CarbFeatureGenerator extends FeatureGenerator {}

/** Contains methods to generate [[VerifierFeature]]s about a programs syntactic properties */
class ProgramSyntaxProperties(val program: String, val pp: PProgram) {

  def getFeatures: Seq[VerifierFeature] = Seq(
    VerifierFeature("hasSet", hasSet.toString),
    VerifierFeature("hasSeq", hasSeq.toString),
    VerifierFeature("hasMagicWand", hasMagicWand.toString),
    VerifierFeature("hasWildcardPerm", hasWildcardPerm.toString),
    VerifierFeature("hasPerm", hasPerm.toString),
    VerifierFeature("hasForPerm", hasForPerm.toString),
    VerifierFeature("hasTermination", hasTermination.toString),
    VerifierFeature("hasRecursivePred", hasRecursivePred.toString),
    VerifierFeature("hasRecursiveFunc", hasRecursiveFunc.toString),
    VerifierFeature("hasMissingTrigger", hasMissingTrigger.toString),
    VerifierFeature("mightHaveQP", mightHaveQP.toString)
  )
  private def programTrees: Seq[Seq[PNode]] =
    Seq(pp.extensions, pp.predicates, pp.methods, pp.fields, pp.domains, pp.functions)

  /** Returns whether a [[PNode]] matching [[pred]] is present in the subtree of [[root]] */
  private def findNode(pred: PNode => Boolean, root: PNode): Boolean =
    pred(root) || Nodes.subnodes(root).exists(findNode(pred, _))

  def hasSet: Boolean =
    PatternMatcher.matchesAtLeastOne(program, Seq("^.*Set\\[.*\\].*$", "^.*Multiset\\[.*\\].*$"), Pattern.MULTILINE)
  def hasSeq: Boolean       = PatternMatcher.doesMatch(program, "^.*Seq\\[.*\\].*$", Pattern.MULTILINE)
  def hasMagicWand: Boolean = PatternMatcher.doesMatch(program, "^.*--\\*.*$", Pattern.MULTILINE)

  def hasWildcardPerm: Boolean = PatternMatcher.doesMatch(program, "^.*wildcard.*$", Pattern.MULTILINE)

  def hasPerm: Boolean = PatternMatcher.matchesAtLeastOne(
    program,
    Seq("^.*wildcard.*$", "^.*perm.*$", "^.*none.*$", "^.*epsilon.*$", "^.*write.*$"),
    Pattern.MULTILINE
  )
  def hasForPerm: Boolean     = PatternMatcher.doesMatch(program, "^.*forperm.*$", Pattern.MULTILINE)
  def hasTermination: Boolean = PatternMatcher.doesMatch(program, "^.*decreases.*$", Pattern.MULTILINE)

  def mightHaveQP: Boolean = {
    programTrees exists { pSeq =>
      pSeq exists { f =>
        {
          findNode(n => n.isInstanceOf[PQuantifier] && findNode(sn => mightBePerm(sn), n), f)
        }
      }
    }
  }

  private def mightBePerm(pn: PNode): Boolean = pn match {
    case _: PNoPerm | _: PWildcard | _: PFullPerm | _: PEpsilon | _: PCurPerm | _: PCall | _: PAccPred => true
    case _                                                                                             => false
  }
  def hasRecursivePred: Boolean = {
    pp.predicates exists { pred =>
      findNode(isCallWithName(pred.idndef.name), pred)
    }
  }
  def hasRecursiveFunc: Boolean = {
    pp.functions exists { f =>
      findNode(isCallWithName(f.idndef.name), f)
    }
  }

  private def isCallWithName(name: String)(pn: PNode): Boolean =
    pn.isInstanceOf[PCall] && pn.asInstanceOf[PCall].func.name == name

  def hasMissingTrigger: Boolean = {
    lazy val res = programTrees flatMap { pSeq =>
      pSeq map { n => findNode(isQuantWOTrigger, n) }
    }
    res.exists(identity)
  }

  private def isQuantWOTrigger(pn: PNode): Boolean =
    pn.isInstanceOf[PQuantifier] && pn.asInstanceOf[PQuantifier].triggers == Seq.empty

}

/** Reporter that stores the runtimes of received [[BenchmarkingPhase]]s */
case class BenchmarkingResultReporter(name: String = "benchmarking_result_reporter") extends Reporter {
  private val _initial_time                = System.currentTimeMillis()
  private var _previous_phase: Long        = _initial_time
  private var results: Seq[(String, Long)] = Seq()

  def report(msg: Message): Unit =
    msg match {
      case BenchmarkingPhase(phase) =>
        val t = System.currentTimeMillis()
        results = results :+ (phase, t - _previous_phase)
        _previous_phase = t
      case _ =>
    }

  def getBenchmarkResults: Seq[(String, Long)] = results
}

case class VerifierFeature(name: String, value: String) extends Serializable
