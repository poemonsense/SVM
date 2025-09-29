package svm

import chisel3._
import chisel3.util._
import difftest.{ArchFpRegState, ArchIntRegState, FullCSRState}
import svm.definitions.RVInstruction

// See riscv-isa-sim/riscv/processor.h for all architectural states
class ArchState extends Bundle {
  val pc = UInt(64.W)
  val xpr = new ArchIntRegState
  val fpr = new ArchFpRegState
  val csr = new FullCSRState

  def fsEnabled: Bool = csr.mstatus(14, 13) =/= 0.U
}

class SVMPipeline(implicit p: SVMParams) extends Bundle {
  val physical_pc = UInt(64.W)
  val is_rvc = Bool()
  val instr = UInt(32.W)

  val xpr = Valid(new Bundle {
    val dest = UInt(5.W)
    val data = UInt(64.W)
  })

  val fpr = Valid(new Bundle {
    val dest = UInt(5.W)
    val data = UInt(64.W)
  })

  val lsu = Valid(new Bundle {
    val opcode = LSUOpcode()
    val virt_addr = UInt(64.W)
    val phy_addr = UInt(64.W)
    val load_data = UInt(64.W)
    val store_data = UInt(64.W)
    val sc_success = Bool()
  })

  val npc = UInt(64.W)

  val exception = Vec(16, Bool())
  val tval = UInt(64.W)

  lazy val rv: RVInstruction = new RVInstruction(instr)
}

class ExecutionHint extends Bundle {
  val skip = Valid(new Bundle {
    val wen = Bool()
    val dest = UInt(5.W)
    val data = UInt(64.W)
  })
  val interrupt = UInt(4.W)
  val is_instr_page_fault = Bool()
  val is_load_page_fault = Bool()
  val is_store_page_fault = Bool()
  val div_data = UInt(64.W)
  val data_paddr = Valid(UInt(64.W))
  val instr = UInt(32.W)
  val is_rvc = Bool()

  def sc_failed: Bool = skip.bits.data =/= 0.U
  def load_data: UInt = skip.bits.data
}

class ReferenceFlags extends Bundle {
  val miss_immu = Bool()
  val miss_fetch = Bool()
  val miss_load = Bool()
  val miss_dmmu = Bool()
  val exit = Bool()

  def is_miss: Bool = miss_immu || miss_fetch || miss_load || miss_dmmu
}

class MicroOp(implicit p: SVMParams) extends Bundle {
  val flags = new ReferenceFlags
  val state = new ArchState
  val hint = new ExecutionHint
  val pipe = new SVMPipeline

  def rv: RVInstruction = pipe.rv
  def snpc: UInt = SignExt(state.pc + Mux(pipe.is_rvc, 2.U, 4.U), 64)

  def has_trap: Bool = pipe.exception.asUInt.orR || hint.interrupt =/= 0.U
  def may_execute: Bool = !hint.skip.valid
  def should_execute: Bool = may_execute && !has_trap

  override def toPrintable: Printable = {
    val pc_p = p"pc ${Hexadecimal(state.pc)}"
    val inst_p = p"inst ${Hexadecimal(pipe.instr)}"
    val wen = pipe.xpr.valid || pipe.fpr.valid
    val dst = Mux(pipe.fpr.valid, pipe.fpr.bits.dest, pipe.xpr.bits.dest)
    val data = Mux(pipe.fpr.valid, pipe.fpr.bits.data, pipe.xpr.bits.data)
    val rf_p = p"wen $wen dst $dst data ${Hexadecimal(data)}"
    val skip_p = p"S(${hint.skip.valid})"
    Seq(pc_p, inst_p, rf_p, skip_p).reduce(_ + ", " + _)
  }
}

class PipelineBits(implicit p: SVMParams) extends Bundle {
  val id = UInt(p.idW)
  val uop = Valid(new MicroOp)
  val more = Vec(p.nCommits, Valid(new ExecutionHint))
}
