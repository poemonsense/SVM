package svm

import chisel3._
import chisel3.util._

class Decoder(instructions: Seq[String]) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(instructions.length.W))
  })

  def matched(insn: String, in: UInt): Bool = {
    val insn_cpp = insn.toUpperCase.dotAsUnderscore
    val mask_val = s"MASK_$insn_cpp".macroToUInt(32.W)
    val match_val = s"MATCH_$insn_cpp".macroToUInt(32.W)
    (mask_val & in) === match_val
  }

  io.out := VecInit(instructions.map(insn => matched(insn, io.in))).asUInt
}

object Decoder {
  def apply(instr_val: UInt, instructions: Seq[String]): UInt = {
    val decoder = Module(new Decoder(instructions))
    decoder.io.in := instr_val
    decoder.io.out
  }

  def apply(instr_val: UInt, instructions: String): Bool = {
    apply(instr_val, Seq(instructions)).asBool
  }
}
