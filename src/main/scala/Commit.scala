package svm

import chisel3._
import chisel3.util._

class Commit(implicit p: SVMParams) extends StoreUnit {
  when(in_valid) {
    // Writeback to XPR
    when(!in.has_trap && in.pipe.xpr.valid) {
      out.state.xpr(in.pipe.xpr.bits.dest) := in.pipe.xpr.bits.data
    }
    // Register $0 is read-only zero.
    out.state.xpr(0) := 0.U

    // Writeback to FPR
    when(!in.has_trap && in.pipe.fpr.valid) {
      out.state.fpr(in.pipe.fpr.bits.dest) := in.pipe.fpr.bits.data
    }

    // PC update
    out.state.pc := in.pipe.npc

    Log.ref(p"[${io.in.bits.id}] commit $in")
  }
}
