package dataCollection.customFrontends

import viper.silver.parser.{
  Nodes,
  PEpsilon,
  PForPerm,
  PFullPerm,
  PMultisetType,
  PNoPerm,
  PNode,
  PProgram,
  PSeqType,
  PSetType,
  PWandType,
  PWildcard
}
import viper.silver.reporter.{ BenchmarkingPhase, Message, Reporter }

trait FeatureGenerator {

  /** Any VerifierFeatures returned by this function will be inserted into the database */
  def getFeatures: Seq[VerifierFeature]
}

/** Trait to extend CollectionSiliconFrontend with to generate features while verifying */
trait SilFeatureGenerator extends FeatureGenerator {

  def getBenchmarkResults: Seq[(String, Long)]

  private def benchmarkResToVF(res: Seq[(String, Long)]): Seq[VerifierFeature] =
    res map { case (phase, time) =>
      VerifierFeature(s"BenchmarkingPhase $phase", time.toString, false)
    }
  override def getFeatures: Seq[VerifierFeature] =
    benchmarkResToVF(getBenchmarkResults)
}

/** Trait to extend CollectionCarbonFrontend with to generate features while verifying */
trait CarbFeatureGenerator extends FeatureGenerator {}

class ProgramSyntaxProperties(val pp: PProgram) {
  private def programTrees: Seq[Seq[PNode]] =
    Seq(pp.extensions, pp.predicates, pp.methods, pp.fields, pp.domains, pp.functions)

  // TODO: might have Quantified permission
  // TODO: Sets
  // TODO: Sequences
  // TODO: magic wands
  // TODO: viper termination correctness
  // TODO: recursive preds, functions
  // TODO: wildcard permissions
  // TODO: Perm, forperm
  // TODO: missing triggers

  private def nodeTypeExists(nt: Class[_ <: PNode], root: PNode): Boolean =
    if (root.getClass == nt) true
    else {
      lazy val childRes = Nodes.subnodes(root) map (c => nodeTypeExists(nt, c))
      childRes.exists(identity)
    }

  private def atLeastOneNodeTypeExists(ntSeq: Seq[Class[_ <: PNode]], root: PNode): Boolean = {
    if (ntSeq.contains(root.getClass)) true
    else {
      lazy val childRes = Nodes.subnodes(root) map (c => atLeastOneNodeTypeExists(ntSeq, c))
      childRes.exists(identity)
    }
  }

  def mightHaveQP: Boolean = ???
  def hasSet: Boolean = {
    lazy val res = programTrees flatMap (pSeq =>
      pSeq map (p => atLeastOneNodeTypeExists(Seq(classOf[PSetType], classOf[PMultisetType]), p))
    )
    res.exists(identity)
  }
  def hasSeq: Boolean = {
    lazy val res = programTrees flatMap (pSeq => pSeq map (p => nodeTypeExists(classOf[PSeqType], p)))
    res.exists(identity)
  }
  def hasMagicWand: Boolean = {
    lazy val res = programTrees flatMap (pSeq => pSeq map (p => nodeTypeExists(classOf[PWandType], p)))
    res.exists(identity)
  }
  def hasTermination: Boolean   = ???
  def hasRecursivePred: Boolean = ???
  def hasRecursiveFunc: Boolean = ???
  def hasWildcardPerm: Boolean = {
    lazy val res = programTrees flatMap (pSeq => pSeq map (p => nodeTypeExists(classOf[PWildcard], p)))
    res.exists(identity)
  }
  def hasPerm: Boolean = {
    lazy val res = programTrees flatMap (pSeq =>
      pSeq map (p =>
        atLeastOneNodeTypeExists(Seq(classOf[PNoPerm], classOf[PFullPerm], classOf[PWildcard], classOf[PEpsilon]), p)
      )
    )
    res.exists(identity)
  }
  def hasForPerm: Boolean = {
    lazy val res = programTrees flatMap (pSeq => pSeq map (p => nodeTypeExists(classOf[PForPerm], p)))
    res.exists(identity)
  }
  def hasMissingTrigger: Boolean = ???

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
