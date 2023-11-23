package dataCollection

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

/** Takes a folder name and runs [[dataCollection.ProcessingPipeline.siliconStage]] */
object SiliconStageRunner extends ProcessRunner {

  import ProcessingPipeline.siliconStage

  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    try {
      siliconStage(args(0))
    } catch {
      case _: StageIncompleteException => System.exit(-1)
    }
  }
}

/** Takes a folder name and runs [[dataCollection.ProcessingPipeline.carbonStage]] */
object CarbonStageRunner extends ProcessRunner {

  import ProcessingPipeline.carbonStage

  def main(args: Array[String]): Unit = {
    if (args.length != 1) return
    try {
      carbonStage(args(0))
    } catch {
      case _: StageIncompleteException => System.exit(-1)
    }
  }
}
