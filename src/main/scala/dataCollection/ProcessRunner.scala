package dataCollection

import dataCollection.ProcessingHelper.{generateCarbonResults, generateSiliconResults}
import dataCollection.ProcessingPipeline.verifierStage
import util.StageIncompleteException

/** Represents a runner for jobs that should be called in a new JVM instance to ensure consistency in performance */
trait ProcessRunner {
  def main(args: Array[String]): Unit
}

/** Takes a programEntryId, generates a Silicon benchmark and inserts it into database */
object SiliconBenchmarkRunner extends ProcessRunner {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    ProcessingHelper.silBenchmarkProgramEntry(args(0).toLong)
  }
}

/** Takes a programEntryId, generates a Carbon benchmark and inserts it into database */
object CarbonBenchmarkRunner extends ProcessRunner {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    ProcessingHelper.carbBenchmarkProgramEntry(args(0).toLong)
  }
}

/** Takes a folder name and runs [[dataCollection.ProcessingPipeline.verifierStage]] with [[generateSiliconResults]] */
object SiliconStageRunner extends ProcessRunner {


  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    try {
      verifierStage(args(0), "silRes.bin", generateSiliconResults)
    } catch {
      case _: StageIncompleteException => System.exit(-1)
    }
  }
}

/** Takes a folder name and runs [[dataCollection.ProcessingPipeline.verifierStage]] with [[generateCarbonResults]] */
object CarbonStageRunner extends ProcessRunner {


  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    try {
      verifierStage(args(0), "carbRes.bin", generateCarbonResults)
    } catch {
      case _: StageIncompleteException => System.exit(-1)
    }
  }
}
