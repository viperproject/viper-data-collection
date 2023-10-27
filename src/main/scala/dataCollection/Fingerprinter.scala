package dataCollection

import viper.silver.ast.Position
import viper.silver.parser._

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.io.Source
import scala.math.Ordered.orderingToOrdered
import upickle.default.{macroRW, write, read, ReadWriter => RW}

import java.io.{BufferedWriter, FileReader, FileWriter}
import scala.io.Source.fromFile


object TestRunner extends App {
  private val testFolder = "/Users/simon/code/silicon/src/test/resources/dataCollection/"
  private val fastParser = new FastParser()
  val fp = Fingerprinter

  private val file1 = Paths.get(testFolder + "test.vpr")
  private val string1 = Source.fromInputStream(Files.newInputStream(file1)).mkString

  private val file2 = Paths.get(testFolder + "test2.vpr")
  private val string2 = Source.fromInputStream(Files.newInputStream(file2)).mkString

  private val file3 = Paths.get(testFolder + "test3.vpr")
  private val string3 = Source.fromInputStream(Files.newInputStream(file3)).mkString

  private val file4 = Paths.get(testFolder + "test4.vpr")
  private val string4 = Source.fromInputStream(Files.newInputStream(file4)).mkString

  private val pp1 = fastParser.parse(string1, file1)
  private val pp1_print = fp.fingerprintPProgram(pp1)

  private val pp2 = fastParser.parse(string2, file2)
  private val pp2_print = fp.fingerprintPProgram(pp2)

  private val pp3 = fastParser.parse(string3, file3)
  private val pp3_print = fp.fingerprintPProgram(pp3)

  private val pp4 = fastParser.parse(string4, file4)
  private val pp4_print = fp.fingerprintPProgram(pp4)

  pp1_print.store(testFolder + "prog1.json")
  private val pprint1_clone = ProgramPrint.load(testFolder + "prog1.json")
  println(pprint1_clone)

  // only variable names, whitespaces changed: expected full match
  println(pp1_print.matchTrees(pp2_print))
  // reordered some statements, high match expected
  println(pp1_print.matchTrees(pp3_print))
  // completely different program, low match expected -> remove low weight nodes
  println(pp1_print.matchTrees(pp4_print))
}

/** Represents a structural fingerprint of a [[PNode]]
 *
 * @param weight  is the size of the node's subtree
 * @param hashVal is a hexadecimal hash-value representing the structural properties of the node's subtree.
 *                It is a combination of the node-type and its child-hashes, s.t. structurally identical subtrees have
 *                the same [[hashVal]]
 * */
case class Fingerprint(weight: Int, hashVal: String)
object Fingerprint {
  implicit val rw: RW[Fingerprint] = macroRW
}

/** Tree to store [[Fingerprint]]s of nodes and their children in a structurally similar way to the original program */
case class FPNode(fp: Fingerprint, children: Seq[FPNode]) {

  /** Returns true if this nodes [[Fingerprint]] is present in the tree represented by [[root]]
   *
   * @param root the tree to search for the fingerprint */
  def containedInTree(root: FPNode): Boolean = {
    if (root.fp.weight < this.fp.weight) return false
    if (root.fp == this.fp) return true
    lazy val childResults = root.children map containedInTree
    childResults.exists(identity)
  }
}
object FPNode {
  implicit val rw: RW[FPNode] = macroRW
}

/** Descending ordering by [[weight]], then [[hashVal]] of the [[FPNode]]'s fingerprint */
object FPNodeOrdering extends Ordering[FPNode] {
  override def compare(x: FPNode, y: FPNode): Int = -((x.fp.weight, x.fp.hashVal) compare(y.fp.weight, y.fp.hashVal))
}

/** Stores the [[FPNode]] tree of a program, split into subtrees for domains, fields, functions, predicates, methods and extensions
 * and provides helper methods to analyze it */
case class ProgramPrint(domainTree: FPNode,
                        fieldTree: FPNode,
                        functionTree: FPNode,
                        predicateTree: FPNode,
                        methodTree: FPNode,
                        extensionTree: FPNode) {
  def matchTrees(oPP: ProgramPrint): MatchResult = {
    MatchResult((numMatches(this.domainTree, oPP.domainTree), domainTree.fp.weight -1),
      (numMatches(this.fieldTree, oPP.fieldTree), fieldTree.fp.weight -1),
      (numMatches(this.functionTree, oPP.functionTree), functionTree.fp.weight -1),
      (numMatches(this.predicateTree, oPP.predicateTree), predicateTree.fp.weight -1),
      (numMatches(this.methodTree, oPP.methodTree), methodTree.fp.weight -1),
      (numMatches(this.extensionTree, oPP.extensionTree), extensionTree.fp.weight -1))
  } // -1 to the tree weights to discount dummy root nodes

  def store(p: String): Unit = {
    val pprintJSON = write(this)
    val w = new BufferedWriter(new FileWriter(p))
    w.write(pprintJSON)
    w.close()
  }

  /** Count of how many nodes in tree of [[currNode]] are present in tree of [[root]]*/
  private def numMatches(root:FPNode, otherRoot: FPNode): Int = {
    val bias = if(root.containedInTree(otherRoot)) -1 else 0 // do not match dummy root node
    val matchCount = numSubTreeMatches(root, otherRoot)
    matchCount + bias
  }

  private def numSubTreeMatches(currNode: FPNode, root: FPNode): Int = {
    if (currNode.containedInTree(root)) return currNode.fp.weight
    currNode.children.map(c => numSubTreeMatches(c, root)).sum
  }
}
object ProgramPrint {
  implicit val rw: RW[ProgramPrint] = macroRW

  def load(p: String): ProgramPrint = {
    val source = fromFile(p)
    val pprintJSON = try source.mkString finally source.close()
    read(pprintJSON)
  }
}


/** Represents results of the matching of two Programs, each tuple contains the amount of nodes that were matched and then the total amount of nodes */
case class MatchResult(dMatches: (Int, Int), fMatches: (Int, Int), funMatches: (Int, Int), pMatches: (Int, Int), mMatches: (Int, Int), extMatches: (Int, Int)) {

  def methodMatchPercentage: Double = {
    100.0 * (mMatches._1.toDouble / mMatches._2.toDouble)
  }

  def preambleMatchPercentage: Double = {
    val numMatches = (Seq(dMatches, fMatches, funMatches, pMatches, extMatches) map (_._1)).sum.toDouble
    val numNodes = (Seq(dMatches, fMatches, funMatches, pMatches, extMatches) map (_._2)).sum
    if (numNodes == 0) 100.0 else 100.0 * (numMatches / numNodes)
  }
  def totalPercentage: Double = {
    100.0 * ((Seq(dMatches, fMatches, funMatches, pMatches, mMatches) map (_._1)).sum.toDouble / (Seq(dMatches, fMatches, funMatches, pMatches, mMatches) map (_._2)).sum)
  }

  override def toString: String = {
    f"""The following number of subtrees were matched:
    Domain: ${dMatches._1} out of ${dMatches._2}
    Field: ${fMatches._1} out of ${fMatches._2}
    Function: ${funMatches._1} out of ${funMatches._2}
    Predicate: ${pMatches._1} out of ${pMatches._2}
    Method: ${mMatches._1} out of ${mMatches._2}
    Extension: ${extMatches._1} out of ${extMatches._2}
    Total: $totalPercentage%1.2f%%"""
  }
}

/** Used to group together sequence of parent-less [[PNode]]s, such as when splitting a program into its fields */
case class RootPNode(childNodes: Seq[PNode])(val pos: (Position, Position)) extends PNode


object Fingerprinter {
  /** Processes a [[PProgram]] and returns a [[FPNode]] tree, representing fingerprints of the program nodes.
   * Fingerprints are only kept for program subtrees of a minimum weight, to reduce memory usage and accidental matches.
   * Child Nodes are sorted by weight, not original order.
   * */
  def fingerprintPProgram(pp: PProgram): ProgramPrint = {
    ProgramPrint(domainTree = trimTree(fingerprintPNode(RootPNode(pp.domains)(null))),
      fieldTree = trimTree(fingerprintPNode(RootPNode(pp.fields)(null))),
      functionTree = trimTree(fingerprintPNode(RootPNode(pp.functions)(null))),
      predicateTree = trimTree(fingerprintPNode(RootPNode(pp.predicates)(null))),
      methodTree = trimTree(fingerprintPNode(RootPNode(pp.methods)(null))),
      extensionTree = trimTree(fingerprintPNode(RootPNode(pp.extensions)(null))))
  }

  private def fingerprintPNode(pn: PNode): FPNode =  {
    val childPrints = subnodes(pn) map fingerprintPNode
    val currHash = hashNode(pn, childPrints)
    val treeWeight = childPrints.map(_.fp.weight).sum + 1
    FPNode(Fingerprint(treeWeight, currHash), childPrints.sorted(FPNodeOrdering))
  }

  private def hashNode(pn: PNode, childPrints: Seq[FPNode]): String = {
    val cPrints = if (isCommutative(pn)) childPrints.sorted(FPNodeOrdering) else childPrints
    val concatHashes: String = ConstNodeHashes.hashValue(pn) + String.valueOf(cPrints flatMap (_.fp.hashVal))
    MD5.generateHash(concatHashes)
  }

  /** Removes all nodes with weight < 4, updates weights */
  private def trimTree(root: FPNode): FPNode = {
    val trimmedTree = dropSmallNodes(root, 3)
    updateWeights(trimmedTree)
  }

  private def dropSmallNodes(root: FPNode, limit: Int): FPNode = {
    val newChildren = root.children map (c => dropSmallNodes(c, limit))
    val largeChildren = newChildren.filter(_.fp.weight > limit)
    FPNode(root.fp, largeChildren)
  }

  private def updateWeights(root: FPNode): FPNode = {
    val updatedChildren = root.children map updateWeights
    FPNode(Fingerprint(updatedChildren.map(_.fp.weight).sum + 1, root.fp.hashVal), updatedChildren.sorted(FPNodeOrdering))
  }

  private def subnodes(pn: PNode): Seq[PNode] = pn match {
    case RootPNode(c) => c
    case _ => Nodes.subnodes(pn)
  }

  private def isCommutative(pn: PNode): Boolean = pn match {
    case PBinExp(_, op, _) => op match {
      case "union" | "intersection" | "+" | "*" | "==" | "!=" | "&&" | "||" | "<==>" => true
      case _ => false
    }
    case _ => false
  }
}

object ConstNodeHashes {
  /** Lookup-table for default hash-values for all types of [[PNode]]
   * */
  def hashValue(p: PNode): String = p match {
    case RootPNode(_) => "e48e13207341b6bf"
    case PIdnDef(_) => "cfcd208495d565ef"
    case PIdnUse(_) => "c4ca4238a0b92382"
    case PUnnamedFormalArgDecl(_) => "c81e728d9d4c2f63"
    case PFormalArgDecl(_, _) => "eccbc87e4b5ce2fe"
    case PFormalReturnDecl(_, _) => "a87ff679a2f3e71d"
    case PLogicalVarDecl(_, _) => "e4da3b7fbbce2345"
    case PLocalVarDecl(_, _) => "1679091c5a880faf"
    case PFieldDecl(_, _) => "8f14e45fceea167a"
    case PPrimitiv(typ) => MD5.generateHash("c9f0f895fb98ab91" + typ)
    case PDomainType(_, _) => "45c48cce2e2d7fbd"
    case PSeqType(_) => "d3d9446802a44259"
    case PSetType(_) => "6512bd43d9caa6e0"
    case PMultisetType(_) => "c20ad4d76fe97759"
    case PMapType(_, _) => "c51ce410c124a10e"
    case PUnknown() => "aab3238922bcc25a"
    case PPredicateType() => "9bf31c7ff062936a"
    case PWandType() => "c74d97b01eae257e"
    case PFunctionType(_, _) => "70efdf2ec9b08607"
    case PBinExp(_, op, _) => MD5.generateHash("6f4922f45568161a" + op)
    case PMagicWandExp(_, _) => "1f0e3dad99908345"
    case PUnExp(op, _) => MD5.generateHash("98f13708210194c4" + op)
    case PTrigger(_) => "3c59dc048e885024"
    case PIntLit(_) => "b6d767d2f8ed5d21"
    case PBoolLit(_) => "37693cfc748049e4"
    case PNullLit() => "1ff1de774005f8da"
    case PResultLit() => "8e296a067a375633"
    case PFieldAccess(_, _) => "4e732ced3463d06d"
    case PCall(_, _, _) => "02e74f10e0327ad8"
    case PUnfolding(_, _) => "33e75ff09dd601bb"
    case PApplying(_, _) => "6ea9ab1baa0efb9e"
    case PExists(_, _, _) => "34173cb38f07f89d"
    case PLabelledOld(_, _) => "c16a5320fa475530"
    case _: POldExp => "6364d3f0f495b6ab"
    case PLet(_, _) => "182be0c5cdcd5072"
    case PLetNestedScope(_, _) => "e369853df766fa44"
    case PForall(_, _, _) => "1c383cd30b7c298a"
    case PForPerm(_, _, _) => "19ca14e7ea6328a4"
    case PCondExp(_, _, _) => "a5bfc9e07964f8dd"
    case PInhaleExhaleExp(_, _) => "a5771bce93e200c3"
    case PCurPerm(_) => "d67d8ab4f4c10bf2"
    case PNoPerm() => "d645920e395fedad"
    case PFullPerm() => "3416a75f4cea9109"
    case PWildcard() => "a1d0c6e83f027327"
    case PEpsilon() => "17e62166fc8586df"
    case PAccPred(_, _) => "f7177163c833dff4"
    case PEmptySeq(_) => "6c8349cc7260ae62"
    case PSeqIndex(_, _) => "d9d4f495e875a2e0"
    case PExplicitSeq(_) => "67c6a1e7ce56d3d6"
    case PRangeSeq(_, _) => "642e92efb7942173"
    case PSeqTake(_, _) => "f457c545a9ded88f"
    case PSeqDrop(_, _) => "c0c7c76d30bd3dca"
    case PSeqUpdate(_, _, _) => "2838023a778dfaec"
    case PLookup(_, _) => "9a1158154dfa42ca"
    case PUpdate(_, _, _) => "d82c8d1619ad8176"
    case PSize(_) => "a684eceee76fc522"
    case PEmptySet(_) => "b53b3a3d6ab90ce0"
    case PExplicitSet(_) => "9f61408e3afb633e"
    case PEmptyMultiset(_) => "72b32a1f754ba1c0"
    case PExplicitMultiset(_) => "66f041e16a60928b"
    case PEmptyMap(_, _) => "093f65e080a295f8"
    case PExplicitMap(_) => "072b030ba126b2f4"
    case PMapRange(_) => "7f39f8317fbdb198"
    case PMapDomain(_) => "44f683a84163b352"
    case PMaplet(_, _) => "03afdbd66e7929b1"
    case PSeqn(_) => "ea5d2f1c4608232e"
    case PFold(_) => "fc490ca45c00b124"
    case PUnfold(_) => "3295c76acbf4caae"
    case PPackageWand(_, _) => "735b90b4568125ed"
    case PApplyWand(_) => "a3f390d88e4c41f2"
    case PExhale(_) => "14bfa6bb14875e45"
    case PAssert(_) => "7cbbc409ec990f19"
    case PInhale(_) => "e2c420d928d4bf8c"
    case PAssume(_) => "32bb90e8976aab52"
    case PNewExp(_) => "d2ddea18f00665ce"
    case PLabel(_, _) => "ad61ab143223efbc"
    case PGoto(_) => "d09bf41544a3365a"
    case PAssign(_, _) => "fbd7939d674997cd"
    case PIf(_, _, _) => "28dd2c7955ce9264"
    case PWhile(_, _, _) => "35f4a8d465e6e1ed"
    case PVars(_, _) => "d1fe173d08e95939"
    case PProgram(_, _, _, _, _, _, _, _, _) => "f033ab37c30201f7"
    case PLocalImport(_) => "43ec517d68b6edd3"
    case PStandardImport(_) => "9778d5d219c5080b"
    case PDomain(_, _, _, _, _) => "fe9fc289c3ff0af1"
    case PFields(_) => "68d30a9594728bc3"
    case PMethod(_, _, _, _, _, _) => "3ef815416f775098"
    case PFunction(_, _, _, _, _, _) => "93db85ed909c1383"
    case PDomainFunction(_, _, _, _, _) => "c7e1249ffc03eb9d"
    case PPredicate(_, _, _) => "2a38a4a9316c49e5"
    case PAxiom(_, _) => "7647966b7343c290"
    case PTypeVarDecl(_) => "8613985ec49eb8f7"
    case PDefine(_, _, _) => "54229abfcfa5649e"
    case PQuasihavoc(_, _) => "92cc227532d17e56"
    case PQuasihavocall(_, _, _) => "98dce83da57b0395"
    case PAnnotatedExp(_, _) => "f4b9ec30ad9f68f8"
    case PAnnotatedStmt(_, _) => "812b4ba287f5ee0b"
    case _: PExtender => "26657d5ff9020d2a"
    case _: PSkip => "e2ef524fbf3d9fe6"
  }
}

object MD5 {
  private val md5Instance: MessageDigest = MessageDigest.getInstance("MD5")

  def generateHash(str: String): String = {
    MD5.md5Instance.digest(str.getBytes()).map("%02x".format(_)).mkString.substring(0, 16)
  }
}