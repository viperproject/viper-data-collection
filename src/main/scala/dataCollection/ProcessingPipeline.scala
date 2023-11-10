package dataCollection

import java.io.{File, FileOutputStream, FileWriter}
import scala.io.Source.fromFile

object ProcessingPipeline {
  import ProcessingHelper._
  import database.BinarySerializer._

  def submissionToTempEntry(): Unit = {
    val entryOpt = processOldestSubmission()
    entryOpt match {
      case Some(entry) => {
        val tmpFileName = entry.hashCode() + "_programEntry"
        val entryBin = serialize(entry)
        val fWriter = new FileOutputStream(new File(tmpFileName))
        try {
          fWriter.write(entryBin)
        } finally {
          fWriter.close()
        }
      }
      case None => ()
    }
  }

}
