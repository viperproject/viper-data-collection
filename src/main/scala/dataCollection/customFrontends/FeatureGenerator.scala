package dataCollection.customFrontends

import database.tools.PatternMatcher
import viper.silver.parser.{
  Nodes,
  PAccPred,
  PCall,
  PCurPerm,
  PEpsilon,
  PFullPerm,
  PNoPerm,
  PNode,
  PProgram,
  PQuantifier,
  PWildcard
}
import viper.silver.reporter.{ BenchmarkingPhase, Message, Reporter }

import java.nio.file.Paths
import java.util.regex.Pattern

trait FeatureGenerator {
  val syntaxProps: ProgramSyntaxProperties

  /** Any VerifierFeatures returned by this function will be inserted into the database */
  def getFeatures: Seq[VerifierFeature] = syntaxProps.getFeatures
}

/** Trait to extend CollectionSiliconFrontend with to generate features while verifying */
trait SilFeatureGenerator extends FeatureGenerator {

  def getBenchmarkResults: Seq[(String, Long)]

  private def benchmarkResToVF(res: Seq[(String, Long)]): Seq[VerifierFeature] =
    res map { case (phase, time) =>
      VerifierFeature(s"BenchmarkingPhase $phase", time.toString, false)
    }
  override def getFeatures: Seq[VerifierFeature] = {
    super.getFeatures ++ benchmarkResToVF(getBenchmarkResults)
  }
}

/** Trait to extend CollectionCarbonFrontend with to generate features while verifying */
trait CarbFeatureGenerator extends FeatureGenerator {}

class ProgramSyntaxProperties(val program: String, val pp: PProgram) {

  def getFeatures: Seq[VerifierFeature] = Seq(
    VerifierFeature("hasSet", hasSet.toString, false),
    VerifierFeature("hasSeq", hasSeq.toString, false),
    VerifierFeature("hasMagicWand", hasMagicWand.toString, false),
    VerifierFeature("hasWildcardPerm", hasWildcardPerm.toString, false),
    VerifierFeature("hasPerm", hasPerm.toString, false),
    VerifierFeature("hasForPerm", hasForPerm.toString, false),
    VerifierFeature("hasTermination", hasTermination.toString, false),
    VerifierFeature("hasRecursivePred", hasRecursivePred.toString, false),
    VerifierFeature("hasRecursiveFunc", hasRecursiveFunc.toString, false),
    VerifierFeature("hasMissingTrigger", hasMissingTrigger.toString, false),
    VerifierFeature("mightHaveQP", mightHaveQP.toString, false)
  )
  private def programTrees: Seq[Seq[PNode]] =
    Seq(pp.extensions, pp.predicates, pp.methods, pp.fields, pp.domains, pp.functions)

  private def findNode(pred: PNode => Boolean, root: PNode): Boolean = {
    if (pred(root)) true
    else {
      lazy val childRes = Nodes.subnodes(root) map (c => findNode(pred, c))
      childRes.exists(identity)
    }
  }

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
    val res = (pp.domains ++ pp.functions ++ pp.methods) map { f =>
      {
        findNode(n => n.isInstanceOf[PQuantifier], f) && findNode(mightBePerm, f)
      }
    }
    res.exists(identity)
  }

  private def mightBePerm(pn: PNode): Boolean = pn match {
    case _: PNoPerm | _: PWildcard | _: PFullPerm | _: PEpsilon | _: PCurPerm | _: PCall | _: PAccPred => true
    case _                                                                                             => false
  }
  def hasRecursivePred: Boolean = {
    lazy val res = pp.predicates map { pred =>
      findNode(isCallWithName(pred.idndef.name), pred)
    }
    res.exists(identity)
  }
  def hasRecursiveFunc: Boolean = {
    lazy val res = pp.functions map { f =>
      findNode(isCallWithName(f.idndef.name), f)
    }
    res.exists(identity)
  }

  private def isCallWithName(name: String)(pn: PNode): Boolean =
    pn.getClass == classOf[PCall] && pn.asInstanceOf[PCall].func.name == name

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

case class VerifierFeature(name: String, value: String, useForFiltering: Boolean) extends Serializable
