package webAPI

import dataCollection.{FPNode, Fingerprint, ProgramPrint}
import upickle.default.{macroRW, ReadWriter => RW}

/** Upickle read writers for serializable objects, more can be found in [[queryFrontend.JSONReadWriters]] */
object JSONReadWriters {
  implicit val ppRW: RW[ProgramPrint]        = macroRW
  implicit val fpnRW: RW[FPNode]             = macroRW
  implicit val fpRW: RW[Fingerprint]         = macroRW
}
