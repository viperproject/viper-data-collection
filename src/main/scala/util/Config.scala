package util

import scala.concurrent.duration.{Duration, MILLISECONDS}

/** Contains constant parameters used in viper-data-collection*/
object Config {
  /** Number of threads to use for database operations*/
  val DB_EXEC_CONTEXT_PARALLELISM: Int = 8
  /** Used as the default timeout parameter for most database queries*/
  val DEFAULT_DB_TIMEOUT: Duration = Duration(10000, MILLISECONDS)
  /** How much more time to allot to benchmarking stages compared to original runtime */
  val BENCHMARK_TIMEOUT_MULTIPLIER: Double = 2.0
  /** Subtrees at or below this weight will be dropped from the fingerprint trees*/
  val FP_TREE_DROP_LIMIT: Int = 3
  /** Percentage of fingerprint nodes that have to be matched for two Viper programs to count as similar */
  val VIPER_MATCH_THRESHOLD: Int = 70
  /** Percentage of fingerprint nodes that have to be matched for two frontend programs to count as similar */
  val FRONTEND_MATCH_THRESHOLD: Int = 70
  /** Fraction representing that a certain feature is valuable if the less than the fraction of entries in the DB contain this feature*/
  val FEATURE_FILTER_THRESHOLD: Double = 0.5
  /** Fraction representing how many of a programs features need to be valuable for the program not to be filtered out*/
  val FEATURE_AMOUNT_THRESHOLD: Double = 1.0 / 3.0

}
