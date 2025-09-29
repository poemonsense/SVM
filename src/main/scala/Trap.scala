package svm

import chisel3._
import chisel3.util._

class Trap(implicit p: SVMParams) extends PipelineStage {
  io.out := io.in

  // Handle the traps. See processor_t::take_trap in riscv-isa-sim/riscv/processor.cc
  val priv = in.state.csr.privilegeMode(1, 0)
  val interrupt = in.hint.interrupt.orR
  val exception = in.pipe.exception.asUInt
  assert(!in_valid || PopCount(exception) <= 1.U, "multiple exceptions cannot be handled")

  when(in_valid && in.has_trap && in.may_execute) {
    val cause = Mux(interrupt, in.hint.interrupt, OHToUInt(exception))
    val cause_val = Cat(interrupt, 0.U(59.W), cause)
    val less_than_S = priv <= u"PRV_S"
    val deleg = Mux(less_than_S, Mux(interrupt, in.state.csr.mideleg, in.state.csr.medeleg)(15, 0), 0.U(16.W))
    when(less_than_S && VecInit(deleg.asBools)(cause)) {
      val vector = Mux(in.state.csr.stvec(0) && interrupt, cause << 2, 0.U)
      out.pipe.npc := Cat(in.state.csr.stvec(63, 1), false.B) + vector
      out.state.csr.scause := cause_val
      out.state.csr.sepc := in.state.pc
      out.state.csr.stval := Mux(interrupt, 0.U, in.pipe.tval)
      val statusUpdateFields = Seq(
        ("MSTATUS_SPIE", get_field(in.state.csr.sstatus, "MSTATUS_SIE")),
        ("MSTATUS_SPP", priv),
        ("MSTATUS_SIE", 0.U),
      )
      out.state.csr.sstatus := set_field(in.state.csr.sstatus, statusUpdateFields)
      out.state.csr.mstatus := set_field(in.state.csr.mstatus, statusUpdateFields)
      out.state.csr.privilegeMode := u"PRV_S"
    }.otherwise {
      val vector = Mux(in.state.csr.mtvec(0) && interrupt, cause << 2, 0.U)
      out.pipe.npc := Cat(in.state.csr.mtvec(63, 1), false.B) + vector
      out.state.csr.mcause := cause_val
      out.state.csr.mepc := in.state.pc
      out.state.csr.mtval := Mux(interrupt, 0.U, in.pipe.tval)
      val statusUpdateFields = Seq(
        ("MSTATUS_MPIE", get_field(in.state.csr.mstatus, "MSTATUS_MIE")),
        ("MSTATUS_MPP", priv),
        ("MSTATUS_MIE", 0.U),
      )
      out.state.csr.mstatus := set_field(in.state.csr.mstatus, statusUpdateFields)
      out.state.csr.privilegeMode := u"PRV_M"
    }
  }
}
