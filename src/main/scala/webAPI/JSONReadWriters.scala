package webAPI

import dataCollection.{FPNode, Fingerprint, ProgramPrint}
import queryFrontend._
import upickle.default.{macroRW, ReadWriter => RW}

import java.sql.Timestamp

/** Upickle read writers for serializable objects, more can be found in [[queryFrontend.JSONReadWriters]] */
object JSONReadWriters {
  implicit val ppRW: RW[ProgramPrint]        = macroRW
  implicit val fpnRW: RW[FPNode]             = macroRW
  implicit val fpRW: RW[Fingerprint]         = macroRW
}
