package dataCollection

import viper.silver.parser.{Nodes, PExists, PForPerm, PForall, PNode, PProgram, PQuantifier}

/* TODO: What features important
  Frontends used
  Verification Results
  Language features: Has domain, predicates, functions, axioms?
  Language features: Quantifiers?




 */
case class ProgramFeatures(id: Int,
                           originalName: String,
                           plaintext: String,
                           loc: Int,
                           frontend: String,
                           originalVerifier: String,
                           siliconRes: Option[VerifierResult],
                           carbonRes: Option[VerifierResult],
                           args: Seq[String],
                           siliconPhaseRuntimes: Seq[(String, Long)],
                           carbonPhaseRuntimes: Seq[(String, Long)],
                           parseSuccess: Boolean,
                           hasPreamble: Boolean,
                           hasQP: Boolean,
                           )

object FeatureExtractor {

  def pnodeHasQP(pn: PNode): Boolean = pn match {
    case PForall(_, _, _) | PExists(_, _, _) | PForPerm(_, _, _) => true
    case _ => {
      lazy val subTreeRes = Nodes.subnodes(pn) map pnodeHasQP
      subTreeRes.exists(identity)
    }
  }

  def hasPreamble(pp: PProgram) = {
    (Seq(pp.predicates, pp.functions, pp.fields, pp.domains, pp.extensions) map (l => l == List())).exists(identity)
  }

  def parsedSuccessfully(pp: PProgram): Boolean = pp.errors match {
    case List() => true
    case _ => false
  }

}
