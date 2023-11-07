package dataCollection

import database.{CarbonResult, ProgramEntry, SiliconResult, UserSubmission}
import viper.silver.parser.FastParser

import java.io.{File, FileWriter}
import java.nio.file.{Path, Paths}
import java.sql.Timestamp
import java.time.{LocalDate, LocalDateTime}
import scala.io.BufferedSource

object ProcessingPipeline {
  private val fastParser = new FastParser()
  private val fPrinter = Fingerprinter

  def createProgramEntryFromSubmission(us: UserSubmission): ProgramEntry = {
    val tmpFile = createTempProgramFile(us.submissionId, us.program)
    val parsedProgram = fastParser.parse(us.program, Paths.get(tmpFile))
    val programPrint = fPrinter.fingerprintPProgram(parsedProgram)
    val parseSuccess = parsedProgram.errors.isEmpty
    val hasPreamble = parsedProgram.predicates.nonEmpty || parsedProgram.domains.nonEmpty || parsedProgram.fields.nonEmpty || parsedProgram.extensions.nonEmpty
    removeTempProgramFile(tmpFile)
    ProgramEntry(0,
      Timestamp.valueOf(LocalDateTime.now()),
      us.originalName,
      us.program,
      us.loc,
      us.frontend,
      us.originalVerifier,
      us.args,
      programPrint,
      parseSuccess,
      hasPreamble,
    )
  }

  def getSiliconResults(pe: ProgramEntry): SiliconResult = {
    val runner = new CollectionSilFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile)
    // original arguments are only used if the program was also originally run with silicon
    if (pe.originalVerifier == "Silicon") {
      args = args ++ pe.args
    }
    runner.runMain(args)
    val runtime = runner.getTime
    val siliconHash = runner.siliconHash
    val phaseRuntimes = runner.getPhaseRuntimes
    val benchmarkResults = runner.getBenchmarkResults
    val success = runner.hasSucceeded
    val errors = runner.errors
    SiliconResult(0,
      siliconHash,
      pe.programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes,
      benchmarkResults
    )
  }

  def getCarbonResults(pe: ProgramEntry): CarbonResult = {
    val runner = new CollectionCarbonFrontend
    val tmpFile = createTempProgramFile(pe.programEntryId, pe.program)
    var args: Array[String] = Array(tmpFile)
    // original arguments are only used if the program was also originally run with carbon
    if (pe.originalVerifier == "Carbon") {
      args = args ++ pe.args
    }
    runner.main(args)
    val runtime = runner.getTime
    val carbonHash = runner.carbonHash
    val phaseRuntimes = runner.getPhaseRuntimes
    val success = runner.hasSucceeded
    val errors = runner.errors
    CarbonResult(0,
      carbonHash,
      pe.programEntryId,
      success,
      runtime,
      errors,
      phaseRuntimes
    )
  }

  def createTempProgramFile(id: Long, program: String): String = {
    val fName = s"./tmp/${id}.vpr"
    val fw: FileWriter = new FileWriter(new File(fName))
    fw.write(program);
    fw.close()
    fName
  }

  def removeTempProgramFile(fName: String): Unit = {
    val f = new File(fName)
    f.delete()
  }
}
