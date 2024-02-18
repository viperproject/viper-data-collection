package dataCollection

import util.Config._
import viper.silver.ast.Position
import viper.silver.parser._

import java.security.MessageDigest
import scala.math.Ordered.orderingToOrdered

trait FingerprintNode {
  def fp: Fingerprint

  def children: Seq[FingerprintNode]
}

/** Trait for a collection of structural fingerprint trees of a program */
trait ProgramFingerprint[T <: FingerprintNode] {
  def domainTree: T

  def fieldTree: T

  def functionTree: T

  def predicateTree: T

  def methodTree: T

  def extensionTree: T

  def numMethods: Int

  def numFunctions: Int

  def trees: Seq[T] = Seq(domainTree, fieldTree, functionTree, predicateTree, methodTree, extensionTree)
}

/** Represents a structural fingerprint of a [[PNode]]
  *
  * @param weight  is the size of the node's subtree
  * @param hashVal is a hexadecimal hash-value representing the structural properties of the node's subtree.
  *                It is a combination of the node-type and its child-hashes, s.t. structurally identical subtrees have
  *                the same [[hashVal]]
  */
case class Fingerprint(weight: Int, hashVal: String)

/** Tree to store [[Fingerprint]]s of nodes and their children in a structurally similar way to the original program */
case class FPNode(fp: Fingerprint, children: Seq[FPNode]) extends FingerprintNode

/** Descending ordering by [[weight]], then [[hashVal]] of the [[FPNode]]'s fingerprint */
object FPNodeOrdering extends Ordering[FPNode] {
  override def compare(x: FPNode, y: FPNode): Int = -((x.fp.weight, x.fp.hashVal) compare (y.fp.weight, y.fp.hashVal))
}

/** Stores the [[FPNode]] tree of a program, split into subtrees for domains, fields, functions, predicates, methods and extensions.
  * This case class is used to serialize and store the Fingerprint information. For comparison purposes, [[ComparableProgramPrint]] is used.
  */
case class ProgramPrint(
  domainTree: FPNode,
  fieldTree: FPNode,
  functionTree: FPNode,
  predicateTree: FPNode,
  methodTree: FPNode,
  extensionTree: FPNode,
  numMethods: Int,
  numFunctions: Int
) extends ProgramFingerprint[FPNode]
    with Serializable

/** Fingerprint Tree Node for comparisons. If a node in this tree is matched to one in another tree, the other node is marked as matched to ensure a 1:1
  * matching between trees. Nodes in this tree aren't marked since they won't be checked more than once.
  */
class ComparableFPNode(fpNode: FPNode, var matched: Boolean = false) extends FingerprintNode {
  val fp: Fingerprint                 = fpNode.fp.copy()
  val children: Seq[ComparableFPNode] = fpNode.children map (c => new ComparableFPNode(c))

  /** Returns true if the tree of [[root]] contains a node with the same Fingerprint, marks that node in the other tree as matched */
  def containedInTree(root: ComparableFPNode): Boolean = {
    if (root.fp.weight < this.fp.weight || root.matched) false
    else if (root.fp == this.fp) {
      root.matched = true
      true
    } else {
      val possibleTrees = root.children.filter(_.fp.weight >= this.fp.weight)
      possibleTrees.exists(containedInTree)
    }
  }

  /** Clears all matched fields in this subtree */
  def clearMatches(): Unit = {
    matched = false
    children foreach (_.clearMatches())
  }
}

/** Class used to compare ProgramPrints. In contrast to a [[ProgramPrint]], this class is mutable and the nodes contain boolean fields to indicate that
  * they were matched to another node, to avoid duplicate matches that could overestimate the similarity of two programs.
  */
class ComparableProgramPrint(pp: ProgramPrint) extends ProgramFingerprint[ComparableFPNode] {
  val domainTree        = new ComparableFPNode(pp.domainTree)
  val fieldTree         = new ComparableFPNode(pp.fieldTree)
  val functionTree      = new ComparableFPNode(pp.functionTree)
  val predicateTree     = new ComparableFPNode(pp.predicateTree)
  val methodTree        = new ComparableFPNode(pp.methodTree)
  val extensionTree     = new ComparableFPNode(pp.extensionTree)
  val numMethods: Int   = pp.numMethods
  val numFunctions: Int = pp.numFunctions

  /** Matches all subtrees of this program to the associated subtrees in [[oPP]]. Clears the [[oPP]] tree after comparison.
    * The dummy parent node is ignored in node weights.
    *
    * @return A [[MatchResult]] containing a tuple per tree with the number of nodes that were matched and total number of nodes.
    */
  def matchTrees(oPP: ComparableProgramPrint): MatchResult = {
    val matchCounts: Seq[Int] = (trees zip oPP.trees) map Function.tupled(countMatchesInTree)
    val matchTuples           = matchCounts zip (trees map (_.fp.weight - 1))
    oPP.clearMatches()

    matchTuples match {
      case Seq(r1, r2, r3, r4, r5, r6) => MatchResult(r1, r2, r3, r4, r5, r6)
      case _                           => throw new IllegalStateException("trees should always return a sequence with 6 elements")
    }
  }

  /** Clears the matched fields in all subtrees of this program. */
  private def clearMatches(): Unit = {
    trees foreach (_.clearMatches())
  }

  /** Returns the amount of nodes in [[root]] that could be matched to one in [[otherRoot]]. This is a separate method to
    * [[countSubTreeMatches]] to discard the dummy parent node.
    */
  private def countMatchesInTree(root: ComparableFPNode, otherRoot: ComparableFPNode): Int = {
    if (root.containedInTree(otherRoot)) return root.fp.weight - 1 // do not match dummy root node
    val matchCount = countSubTreeMatches(root, otherRoot)
    matchCount
  }

  /** Returns the amount of nodes in the tree of [[currNode]] that could be matched to one in [[root]] */
  private def countSubTreeMatches(currNode: ComparableFPNode, root: ComparableFPNode): Int = {
    if (currNode.containedInTree(root)) return currNode.fp.weight
    currNode.children.map(c => countSubTreeMatches(c, root)).sum
  }
}

/** Represents results of the matching of two Programs, each tuple contains the amount of nodes that were matched and then the total amount of nodes of the first program */
case class MatchResult(
  dMatches: (Int, Int) = (1, 1),
  fMatches: (Int, Int) = (1, 1),
  funMatches: (Int, Int) = (1, 1),
  pMatches: (Int, Int) = (1, 1),
  mMatches: (Int, Int) = (1, 1),
  extMatches: (Int, Int) = (1, 1)
) {

  def tuples: Seq[(Int, Int)] = Seq(dMatches, fMatches, funMatches, pMatches, mMatches, extMatches)

  def methMatchP: Double = {
    tupleMatchP(Seq(mMatches))
  }

  def funMatchP: Double = {
    tupleMatchP(Seq(funMatches))
  }

  def methFunMatchP: Double = {
    tupleMatchP(Seq(mMatches, funMatches))
  }

  def preambleMatchP: Double = {
    tupleMatchP(Seq(dMatches, fMatches, pMatches, extMatches))
  }

  def totalMatchP: Double = {
    tupleMatchP(tuples)
  }

  /** Given a sequence of [[(Int, Int)]] tuples, sums the first elements, then the second and returns the fraction between both */
  def tupleMatchP(tups: Seq[(Int, Int)]): Double = {
    val numMatches = (tups map (_._1)).sum.toDouble
    val numNodes   = (tups map (_._2)).sum
    if (numNodes == 0) 0.0 else 100.0 * (numMatches / numNodes)
  }

  def isSubset: Boolean = {
    totalMatchP >= 99.0
  }

  def isViperMatch: Boolean = {
    totalMatchP >= VIPER_MATCH_THRESHOLD
  }

  def isFrontendMatch: Boolean = {
    methFunMatchP >= FRONTEND_MATCH_THRESHOLD
  }

  override def toString: String = {
    f"""The following number of subtrees were matched:
    Domain: ${dMatches._1} out of ${dMatches._2}
    Field: ${fMatches._1} out of ${fMatches._2}
    Function: ${funMatches._1} out of ${funMatches._2}
    Predicate: ${pMatches._1} out of ${pMatches._2}
    Method: ${mMatches._1} out of ${mMatches._2}
    Extension: ${extMatches._1} out of ${extMatches._2}
    Preamble: $preambleMatchP%1.2f%%
    Methods: $methMatchP%1.2f%%
    Total: $totalMatchP%1.2f%%"""
  }
}

/** Used to group together sequence of parent-less [[PNode]]s, such as when splitting a program into its fields */
case class RootPNode(childNodes: Seq[PNode])(val pos: (Position, Position)) extends PNode {
  def pretty: String = ""
}

object Fingerprinter {

  /** Processes a [[PProgram]] and returns a [[FPNode]] tree, representing fingerprints of the program nodes.
    * Fingerprints are only kept for program subtrees of a minimum weight, to reduce memory usage and accidental matches.
    * Child Nodes are sorted by weight, not original order.
    */
  def fingerprintPProgram(pp: PProgram): ProgramPrint = {
    val programTrees = Seq(pp.domains, pp.fields, pp.functions, pp.predicates, pp.methods, pp.extensions)
    val fpTrees      = programTrees map (t => trimTree(fingerprintPNode(RootPNode(t)(null))))
    fpTrees match {
      case Seq(t1, t2, t3, t4, t5, t6) => ProgramPrint(t1, t2, t3, t4, t5, t6, pp.methods.size, pp.functions.size)
      case _                           => throw new IllegalStateException("This sequence should always contain 6 elements")
    }
  }

  /** @return A [[FPNode]] containing the children's [[FPNode]]s and a Fingerprint with the the children's weight,
    *          and a hash value dependent on the nodes children
    */
  private def fingerprintPNode(pn: PNode): FPNode = {
    val flatPN      = flatten(pn)
    val children    = subnodes(flatPN)
    val childPrints = (children._1 map fingerprintPNode) ++ (children._2 map fingerprintPNode).sorted(FPNodeOrdering)
    val currHash    = hashNode(flatPN, childPrints)
    val treeWeight  = childPrints.map(_.fp.weight).sum + 1
    FPNode(Fingerprint(treeWeight, currHash), childPrints)
  }

  private def hashNode(pn: PNode, childPrints: Seq[FPNode]): String = {
    val concatHashes: String = ConstNodeHashes.hashValue(pn) + String.valueOf(childPrints flatMap (_.fp.hashVal))
    MD5.generateHash(concatHashes)
  }

  /** Removes all leaf nodes, updates weights */
  private def trimTree(root: FPNode): FPNode = {
    val trimmedTree = dropSmallNodes(root, FP_TREE_DROP_LIMIT)
    val uwTree      = updateWeights(trimmedTree)
    uwTree
  }

  /** removes all subtrees with <= [[limit]] child nodes */
  private def dropSmallNodes(root: FPNode, limit: Int): FPNode = {
    val newChildren   = root.children map (c => dropSmallNodes(c, limit))
    val largeChildren = newChildren.filter(_.fp.weight > limit)
    FPNode(root.fp, largeChildren)
  }

  /** recalculates the weight of a node, should be called after modifying the tree */
  private def updateWeights(root: FPNode): FPNode = {
    val updatedChildren = root.children map updateWeights
    FPNode(Fingerprint(updatedChildren.map(_.fp.weight).sum + 1, root.fp.hashVal), updatedChildren)
  }

  /** @return Two sequences of child nodes. First contains all regular children, second ones that should be treated as commutative
    */
  private def subnodes(pn: PNode): (Seq[PNode], Seq[PNode]) = pn match {
    // this list is probably incomplete
    case PMethod(_, _, idndef, args, rets, pres, posts, body) =>
      (Seq(idndef, args) ++ Seq(rets, body).flatten, Seq(pres, posts))
    case PFunction(_, _, name, args, _, typ, pres, posts, body) =>
      (Seq(name, args, typ) ++ Seq(body).flatten, Seq(pres, posts))
    case PWhile(_, cond, invs, body) => (Seq(cond) ++ Seq(body.ss), invs.subnodes)
    case PBinExp(l, op, r)           => if (commutativeOps.contains(op)) (Seq(), Seq(l, r)) else (Seq(l, r), Seq())
    case RootPNode(c)                => (c, Seq())
    case _                           => (pn.subnodes, Seq())
  }

  /** Splits pres, posts and invariants of methods, functions and while loops that are combined with && into separate statements */
  private def flatten(pn: PNode): PNode = {
    pn match {
      case wh: PWhile =>
        wh.copy(invs =
          PDelimited[PSpecification[PKw.InvSpec], Option[PSym.Semi]](wh.invs.toSeq flatMap flattenSpec)(wh.invs.pos)
        )(wh.pos)
      case pm: PMethod =>
        pm.copy(
          pres =
            PDelimited[PSpecification[PKw.PreSpec], Option[PSym.Semi]](pm.pres.toSeq flatMap flattenSpec)(pm.pres.pos),
          posts = PDelimited[PSpecification[PKw.PostSpec], Option[PSym.Semi]](pm.posts.toSeq flatMap flattenSpec)(
            pm.posts.pos
          )
        )(
          pm.pos
        )
      case pf: PFunction =>
        pf.copy(
          pres =
            PDelimited[PSpecification[PKw.PreSpec], Option[PSym.Semi]](pf.pres.toSeq flatMap flattenSpec)(pf.pres.pos),
          posts = PDelimited[PSpecification[PKw.PostSpec], Option[PSym.Semi]](pf.posts.toSeq flatMap flattenSpec)(
            pf.posts.pos
          )
        )(
          pf.pos
        )
      case _ => pn
    }
  }

  private def flattenSpec[T <: PKw.Spec](s: PSpecification[T]): Seq[(PSpecification[T], Option[PSym.Semi])] = {
    (flattenBinExpAnd(s.e) map ((e: PExp) => PSpecification[T](s.k, e)(s.pos))) map (e => (e, None))
  }

  private def flattenBinExpAnd(pn: PExp): Seq[PExp] = {
    pn match {
      case bn: PBinExp =>
        if (f"${bn.op}".trim() == "&&") flattenBinExpAnd(bn.left) ++ flattenBinExpAnd(bn.right) else Seq(bn)
      case _ => Seq(pn)
    }
  }

  private def commutativeOps = Seq("union", "intersection", "+", "*", "==", "!=", "&&", "||", "<==>")
}

object ConstNodeHashes {

  /** Lookup-table for default hash-values for all types of [[PNode]]
    */
  def hashValue(p: PNode): String = p match {
    case _: RootPNode      => "e48e13207341b6bf"
    case PPrimitiv(typ)    => MD5.generateHash("c9f0f895fb98ab91" + typ)
    case PBinExp(_, op, _) => MD5.generateHash("6f4922f45568161a" + op)
    case PUnExp(op, _)     => MD5.generateHash("98f13708210194c4" + op)
    case n                 => MD5.generateHash("15b1e5ffe0aaab3c" + n.getClass().toString())
  }
}

object MD5 {
  private val md5Instance: MessageDigest = MessageDigest.getInstance("MD5")

  def generateHash(str: String): String = {
    MD5.md5Instance.digest(str.getBytes()).map("%02x".format(_)).mkString.substring(0, 16)
  }
}
