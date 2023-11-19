package webAPI

import dataCollection.{FPNode, Fingerprint, ProgramPrint}
import database.{CarbonResult, ProgramEntry, SiliconResult}
import upickle.default.{macroRW, ReadWriter => RW}

import java.sql.Timestamp

object JSONReadWriters {
  implicit val peRW: RW[ProgramEntry] = macroRW
  implicit val tsRW: RW[Timestamp] = upickle.default.readwriter[String].bimap[Timestamp](
    t => s"${t.getTime}",
    str => new Timestamp(str.toLong)
  )
  implicit val ppRW: RW[ProgramPrint] = macroRW
  implicit val fpnRW: RW[FPNode] = macroRW
  implicit val fpRW: RW[Fingerprint] = macroRW
  implicit val srRW: RW[SiliconResult] = macroRW
  implicit val crRW: RW[CarbonResult] = macroRW
}
