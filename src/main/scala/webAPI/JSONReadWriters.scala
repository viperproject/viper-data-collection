package webAPI

import dataCollection.{FPNode, Fingerprint, ProgramPrint}
import database.tools.PatternMatchResult
import queryFrontend._
import upickle.default.{macroRW, ReadWriter => RW}

import java.sql.Timestamp

/** Upickle read writers for serializable objects */
object JSONReadWriters {
  implicit val peRW: RW[ProgramEntry] = macroRW
  implicit val tsRW: RW[Timestamp] = upickle.default
    .readwriter[String]
    .bimap[Timestamp](
      t => s"${t.getTime}",
      str => new Timestamp(str.toLong)
    )
  implicit val ppRW: RW[ProgramPrint]        = macroRW
  implicit val fpnRW: RW[FPNode]             = macroRW
  implicit val fpRW: RW[Fingerprint]         = macroRW
  implicit val pmrRW: RW[PatternMatchResult] = macroRW
  implicit val veRW: RW[VerError]            = macroRW
  implicit val vrRW: RW[VerResult]           = macroRW
}
