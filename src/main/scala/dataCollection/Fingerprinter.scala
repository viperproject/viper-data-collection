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
  val fp       = fpNode.fp.copy()
  val children = fpNode.children map (c => new ComparableFPNode(c))

  /** Returns true if the tree of [[root]] contains a node with the same Fingerprint, marks that node in the other tree as matched */
  def containedInTree(root: ComparableFPNode): Boolean = {
    if (root.fp.weight < this.fp.weight || root.matched) false
    else if (root.fp == this.fp) {
      root.matched = true
      true
    } else {
      val possibleTrees = root.children.filter(_.fp.weight >= this.fp.weight)
      for (t <- possibleTrees) {
        if(containedInTree(t)) return true
      }
      false
    }
  }

  /** Clears all matched fields in this subtree */
  def clearMatches: Unit = {
    matched = false
    children foreach (_.clearMatches)
  }
}

/** Class used to compare ProgramPrints. In contrast to a [[ProgramPrint]], this class is mutable and the nodes contain boolean fields to indicate that
  * they were matched to another node, to avoid duplicate matches that could overestimate the similarity of two programs.
  */
class ComparableProgramPrint(pp: ProgramPrint) extends ProgramFingerprint[ComparableFPNode] {
  val domainTree    = new ComparableFPNode(pp.domainTree)
  val fieldTree     = new ComparableFPNode(pp.fieldTree)
  val functionTree  = new ComparableFPNode(pp.functionTree)
  val predicateTree = new ComparableFPNode(pp.predicateTree)
  val methodTree    = new ComparableFPNode(pp.methodTree)
  val extensionTree = new ComparableFPNode(pp.extensionTree)
  val numMethods    = pp.numMethods
  val numFunctions  = pp.numFunctions

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
    trees foreach (_.clearMatches)
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
case class RootPNode(childNodes: Seq[PNode])(val pos: (Position, Position)) extends PNode

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
    case PMethod(idndef, args, rets, pres, posts, body) => (Seq(idndef) ++ args ++ rets ++ body.toSeq, pres ++ posts)
    case PFunction(name, args, typ, pres, posts, body)  => (Seq(name) ++ args ++ Seq(typ) ++ body.toSeq, pres ++ posts)
    case PWhile(cond, invs, body)                       => (Seq(cond) ++ Seq(body), invs)
    case PBinExp(l, op, r)                              => if (commutativeOps.contains(op)) (Seq(), Seq(l, r)) else (Seq(l, r), Seq())
    case RootPNode(c)                                   => (c, Seq())
    case _                                              => (Nodes.subnodes(pn), Seq())
  }

  /** Splits pres, posts and invariants of methods, functions and while loops that are combined with && into separate statements */
  private def flatten(pn: PNode): PNode = {
    pn match {
      case wh: PWhile => wh.copy(invs = wh.invs flatMap flattenBinExpAnd)(wh.pos)
      case pm: PMethod =>
        pm.copy(pres = pm.pres flatMap flattenBinExpAnd, posts = pm.posts flatMap flattenBinExpAnd)(
          pm.pos,
          pm.annotations
        )
      case pf: PFunction =>
        pf.copy(pres = pf.pres flatMap flattenBinExpAnd, posts = pf.posts flatMap flattenBinExpAnd)(
          pf.pos,
          pf.annotations
        )
      case _ => pn
    }
  }

  private def flattenBinExpAnd(pn: PExp): Seq[PExp] = {
    pn match {
      case bn: PBinExp => if (bn.opName == "&&") (flattenBinExpAnd(bn.left) ++ flattenBinExpAnd(bn.right)) else Seq(bn)
      case _           => Seq(pn)
    }
  }

  private def commutativeOps = Seq("union", "intersection", "+", "*", "==", "!=", "&&", "||", "<==>")
}

object ConstNodeHashes {

  /** Lookup-table for default hash-values for all types of [[PNode]]
    */
  def hashValue(p: PNode): String = p match {
    case RootPNode(_)                        => "e48e13207341b6bf"
    case PIdnDef(_)                          => "cfcd208495d565ef"
    case PIdnUse(_)                          => "c4ca4238a0b92382"
    case PUnnamedFormalArgDecl(_)            => "c81e728d9d4c2f63"
    case PFormalArgDecl(_, _)                => "eccbc87e4b5ce2fe"
    case PFormalReturnDecl(_, _)             => "a87ff679a2f3e71d"
    case PLogicalVarDecl(_, _)               => "e4da3b7fbbce2345"
    case PLocalVarDecl(_, _)                 => "1679091c5a880faf"
    case PFieldDecl(_, _)                    => "8f14e45fceea167a"
    case PPrimitiv(typ)                      => MD5.generateHash("c9f0f895fb98ab91" + typ)
    case PDomainType(_, _)                   => "45c48cce2e2d7fbd"
    case PSeqType(_)                         => "d3d9446802a44259"
    case PSetType(_)                         => "6512bd43d9caa6e0"
    case PMultisetType(_)                    => "c20ad4d76fe97759"
    case PMapType(_, _)                      => "c51ce410c124a10e"
    case PUnknown()                          => "aab3238922bcc25a"
    case PPredicateType()                    => "9bf31c7ff062936a"
    case PWandType()                         => "c74d97b01eae257e"
    case PFunctionType(_, _)                 => "70efdf2ec9b08607"
    case PBinExp(_, op, _)                   => MD5.generateHash("6f4922f45568161a" + op)
    case PMagicWandExp(_, _)                 => "1f0e3dad99908345"
    case PUnExp(op, _)                       => MD5.generateHash("98f13708210194c4" + op)
    case PTrigger(_)                         => "3c59dc048e885024"
    case PIntLit(_)                          => "b6d767d2f8ed5d21"
    case PBoolLit(_)                         => "37693cfc748049e4"
    case PNullLit()                          => "1ff1de774005f8da"
    case PResultLit()                        => "8e296a067a375633"
    case PFieldAccess(_, _)                  => "4e732ced3463d06d"
    case PCall(_, _, _)                      => "02e74f10e0327ad8"
    case PUnfolding(_, _)                    => "33e75ff09dd601bb"
    case PApplying(_, _)                     => "6ea9ab1baa0efb9e"
    case PExists(_, _, _)                    => "34173cb38f07f89d"
    case PLabelledOld(_, _)                  => "c16a5320fa475530"
    case _: POldExp                          => "6364d3f0f495b6ab"
    case PLet(_, _)                          => "182be0c5cdcd5072"
    case PLetNestedScope(_, _)               => "e369853df766fa44"
    case PForall(_, _, _)                    => "1c383cd30b7c298a"
    case PForPerm(_, _, _)                   => "19ca14e7ea6328a4"
    case PCondExp(_, _, _)                   => "a5bfc9e07964f8dd"
    case PInhaleExhaleExp(_, _)              => "a5771bce93e200c3"
    case PCurPerm(_)                         => "d67d8ab4f4c10bf2"
    case PNoPerm()                           => "d645920e395fedad"
    case PFullPerm()                         => "3416a75f4cea9109"
    case PWildcard()                         => "a1d0c6e83f027327"
    case PEpsilon()                          => "17e62166fc8586df"
    case PAccPred(_, _)                      => "f7177163c833dff4"
    case PEmptySeq(_)                        => "6c8349cc7260ae62"
    case PSeqIndex(_, _)                     => "d9d4f495e875a2e0"
    case PExplicitSeq(_)                     => "67c6a1e7ce56d3d6"
    case PRangeSeq(_, _)                     => "642e92efb7942173"
    case PSeqTake(_, _)                      => "f457c545a9ded88f"
    case PSeqDrop(_, _)                      => "c0c7c76d30bd3dca"
    case PSeqUpdate(_, _, _)                 => "2838023a778dfaec"
    case PLookup(_, _)                       => "9a1158154dfa42ca"
    case PUpdate(_, _, _)                    => "d82c8d1619ad8176"
    case PSize(_)                            => "a684eceee76fc522"
    case PEmptySet(_)                        => "b53b3a3d6ab90ce0"
    case PExplicitSet(_)                     => "9f61408e3afb633e"
    case PEmptyMultiset(_)                   => "72b32a1f754ba1c0"
    case PExplicitMultiset(_)                => "66f041e16a60928b"
    case PEmptyMap(_, _)                     => "093f65e080a295f8"
    case PExplicitMap(_)                     => "072b030ba126b2f4"
    case PMapRange(_)                        => "7f39f8317fbdb198"
    case PMapDomain(_)                       => "44f683a84163b352"
    case PMaplet(_, _)                       => "03afdbd66e7929b1"
    case PSeqn(_)                            => "ea5d2f1c4608232e"
    case PFold(_)                            => "fc490ca45c00b124"
    case PUnfold(_)                          => "3295c76acbf4caae"
    case PPackageWand(_, _)                  => "735b90b4568125ed"
    case PApplyWand(_)                       => "a3f390d88e4c41f2"
    case PExhale(_)                          => "14bfa6bb14875e45"
    case PAssert(_)                          => "7cbbc409ec990f19"
    case PInhale(_)                          => "e2c420d928d4bf8c"
    case PAssume(_)                          => "32bb90e8976aab52"
    case PNewExp(_)                          => "d2ddea18f00665ce"
    case PLabel(_, _)                        => "ad61ab143223efbc"
    case PGoto(_)                            => "d09bf41544a3365a"
    case PAssign(_, _)                       => "fbd7939d674997cd"
    case PIf(_, _, _)                        => "28dd2c7955ce9264"
    case PWhile(_, _, _)                     => "35f4a8d465e6e1ed"
    case PVars(_, _)                         => "d1fe173d08e95939"
    case PProgram(_, _, _, _, _, _, _, _, _) => "f033ab37c30201f7"
    case PLocalImport(_)                     => "43ec517d68b6edd3"
    case PStandardImport(_)                  => "9778d5d219c5080b"
    case PDomain(_, _, _, _, _)              => "fe9fc289c3ff0af1"
    case PFields(_)                          => "68d30a9594728bc3"
    case PMethod(_, _, _, _, _, _)           => "3ef815416f775098"
    case PFunction(_, _, _, _, _, _)         => "93db85ed909c1383"
    case PDomainFunction(_, _, _, _, _)      => "c7e1249ffc03eb9d"
    case PPredicate(_, _, _)                 => "2a38a4a9316c49e5"
    case PAxiom(_, _)                        => "7647966b7343c290"
    case PTypeVarDecl(_)                     => "8613985ec49eb8f7"
    case PDefine(_, _, _)                    => "54229abfcfa5649e"
    case PQuasihavoc(_, _)                   => "92cc227532d17e56"
    case PQuasihavocall(_, _, _)             => "98dce83da57b0395"
    case PAnnotatedExp(_, _)                 => "f4b9ec30ad9f68f8"
    case PAnnotatedStmt(_, _)                => "812b4ba287f5ee0b"
    case _: PExtender                        => "26657d5ff9020d2a"
    case _: PSkip                            => "e2ef524fbf3d9fe6"
  }
}

object MD5 {
  private val md5Instance: MessageDigest = MessageDigest.getInstance("MD5")

  def generateHash(str: String): String = {
    MD5.md5Instance.digest(str.getBytes()).map("%02x".format(_)).mkString.substring(0, 16)
  }
}
