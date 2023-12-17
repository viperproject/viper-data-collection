package util

import scala.concurrent.duration.{Duration, SECONDS}

/** Contains constant parameters used in viper-data-collection */
object Config {

  /** Number of threads to use for database operations */
  val DB_EXEC_CONTEXT_PARALLELISM: Int = 10

  /** Used as the default timeout parameter for most database queries */
  val DEFAULT_DB_TIMEOUT: Duration = Duration(10, SECONDS)

  /** Default timeout for operations that might take longer than a simple query */
  val LONG_TIMEOUT: Duration = Duration(100, SECONDS)

  /** Number of results to load into memory at once when using streamed queries */
  val DB_BATCH_SIZE: Int = 1000

  /** How much more time to allot to benchmarking stages compared to original runtime */
  val BENCHMARK_TIMEOUT_MULTIPLIER: Double = 5.0

  /** Minimum timeout for a benchmark, might be needed for very low original runtimes, f.e. when caching is involved */
  val MIN_BENCHMARK_TIMEOUT_SECONDS: Int = 10

  /** Subtrees at or below this weight will be dropped from the fingerprint trees */
  val FP_TREE_DROP_LIMIT: Int = 3

  /** Percentage of fingerprint nodes that have to be matched for two Viper programs to count as similar */
  val VIPER_MATCH_THRESHOLD: Int = 70

  /** Percentage of fingerprint nodes that have to be matched for two frontend programs to count as similar */
  val FRONTEND_MATCH_THRESHOLD: Int = 70

  /** Fraction representing that a certain metadata is valuable if the less than the fraction of entries in the DB contain this feature */
  val METADATA_FILTER_THRESHOLD: Double = 0.5

  /** Fraction representing how many of a programs metadata need to be valuable for the program not to be filtered out */
  val METADATA_AMOUNT_THRESHOLD: Double = 1.0 / 3.0

  /** Directory to use to for storing temporary files */
  val TMP_DIRECTORY: String = "./tmp"

  /** Directory where bash scripts are stored */
  val SCRIPT_DIRECTORY: String = "./bash_scripts"

  /** Bash file that executes the given class / object name */
  val SCALA_CLASS_BASH_FILE: String = SCRIPT_DIRECTORY + "/run_scala_class.sh"

  /** Bash file that switches Silicon version to given commit */
  val SWITCH_SIL_VERSION_BASH_FILE: String = SCRIPT_DIRECTORY + "/switch_sil_version.sh"

  /** Bash file that switches Silicon version to given HEAD */
  val RESTORE_SIL_VERSION_BASH_FILE: String = SCRIPT_DIRECTORY + "/restore_sil_version.sh"

  /** Bash file that switches Carbon version to given commit */
  val SWITCH_CARB_VERSION_BASH_FILE: String = SCRIPT_DIRECTORY + "/switch_carb_version.sh"

  /** Bash file that switches Carbon version to HEAD */
  val RESTORE_CARB_VERSION_BASH_FILE: String = SCRIPT_DIRECTORY + "/restore_carb_version.sh"

  /** Bash file that outputs current Carbon commit hash */
  val GET_CARBON_HASH_BASH_FILE: String = SCRIPT_DIRECTORY + "/get_carbon_hash.sh"
}
