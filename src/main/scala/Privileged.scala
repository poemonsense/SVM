package svm

import chisel3._
import chisel3.util._
import svm.definitions.{CSRDef, HasCSRConstants, MstatusCSRDef, RVInstruction, RVInstructions, SstatusCSRDef}

class Privileged(implicit p: SVMParams) extends PipelineStage with HasCSRConstants {

  // Default connections
  io.out := io.in

  // Control bits
  val instr = new RVInstruction(in.pipe.instr)
  val csr_insn_decoded = Decoder(instr.get, RVInstructions.Zicsr)

  val csr_insn = RVInstructions.Zicsr.zip(csr_insn_decoded.asBools)
  val is_csr_write = VecInit(csr_insn.filter(_._1.contains("RW")).map(_._2)).asUInt.orR
  val is_csr_set = VecInit(csr_insn.filter(_._1.contains("RS")).map(_._2)).asUInt.orR && instr.rs1 =/= 0.U
  val is_csr_clear = VecInit(csr_insn.filter(_._1.contains("RC")).map(_._2)).asUInt.orR
  val is_csr_imm = VecInit(csr_insn.filter(_._1.endsWith("I")).map(_._2)).asUInt.orR

  // Avoid CSR operations if this instruction has any of its exception bits set.
  val prior_exception = in.pipe.exception.zipWithIndex.filterNot(_._2 == i"CAUSE_ILLEGAL_INSTRUCTION").map(_._1)
  val has_prior_exception = VecInit(prior_exception).asUInt.orR || in.hint.interrupt =/= 0.U
  val do_csr_operation = in_valid && !in.hint.skip.valid && !has_prior_exception

  val do_csr_read = VecInit(csr_insn.map(_._2)).asUInt.orR
  val do_csr_write = is_csr_write
  val do_csr_set = is_csr_set && instr.rs1 =/= 0.U
  val do_csr_clear = is_csr_clear && instr.rs1 =/= 0.U
  val csr_update_conds = Seq(do_csr_write, do_csr_set, do_csr_clear)
  val csr_update_funcs = Seq((csr: CSRDef) => csr.write _, (csr: CSRDef) => csr.set _, (csr: CSRDef) => csr.clear _)
  // For CSRRSI and CSRRCI, if the uimm[4:0] field is zero, then these instructions will not write to the CSR,
  // and shall not cause any of the side effects that might otherwise occur on a CSR write,
  // nor raise illegal instruction exceptions on accesses to read-only CSRs.
  val csr_update_side_effect = do_csr_write || (do_csr_set || do_csr_clear) && (!is_csr_imm || instr.rs1 =/= 0.U)
  val csr_wdata = Mux(is_csr_imm, ZeroExt(instr.rs1, 64), in.state.xpr(instr.rs1))

  // CSR definitions
  val csrs_diff = in.state.csr.toNameValSeq.zip(out.state.csr.toSeq).tail.flatMap(x => CSRDef(x._1._1, x._1._2, x._2))
  val csrs_internal = CSRDef.csrNames.filterNot(name => csrs_diff.exists(_.is(name))).flatMap(CSRDef.apply)
  val csrs = csrs_diff ++ csrs_internal

  // CSR instructions: read, write, set, clear
  val csrs_hit = csrs.map(_.address.U === instr.csr)
  val is_csr_hit = VecInit(csrs_hit).asUInt.orR

  val csr_rdata = Mux1H(csrs_hit, csrs.map(_.read))
  when(do_csr_operation && do_csr_read && is_csr_hit) {
    out.pipe.xpr.valid := true.B
    out.pipe.xpr.bits.dest := instr.rd
    out.pipe.xpr.bits.data := csr_rdata
  }

  csrs.zip(csrs_hit).foreach { case (csr, hit) =>
    csr_update_conds.zip(csr_update_funcs).foreach { case (cond, func) =>
      when(do_csr_operation && hit && cond) {
        func(csr)(csr_wdata)
        when(csr_update_side_effect) {
          csr.writeSideEffect(csrs)
        }
      }
    }
  }

  val mstatusCsrDef = csrs_diff.find(_.is("MSTATUS")).get.asInstanceOf[MstatusCSRDef]
  when(do_csr_operation && in.pipe.fpr.valid) {
    out.state.csr.mstatus := mstatusCsrDef.adjust_sd(set_field(in.state.csr.mstatus, i"MSTATUS_FS", 3.U))
  }

  // Special fix for sstatus, which is a proxy CSR of mstatus.
  // This is required because the update values of mstatus is not synced to sstatus.
  out.state.csr.sstatus := new SstatusCSRDef(new MstatusCSRDef(Some(out.state.csr.mstatus), None)).read

  // Other privileged instructions
  val system_insn = RVInstructions.system.filterNot(RVInstructions.Zicsr.contains)
  val system_insn_decoded = Decoder(in.pipe.instr, system_insn)
  system_insn_decoded.asBools.zip(system_insn).foreach { case (v, insn) =>
    when(do_csr_operation && v) {
      // call the do_$insn functions, such as do_mret, do_sret, ...
      getClass.getDeclaredMethod(s"do_${insn.toLowerCase.dotAsUnderscore}").invoke(this)
    }
  }

  // Custom NEMU_TRAP instruction
  val is_nemu_trap = (0x707c.U & in.pipe.instr) === 0x0068.U
  when(do_csr_operation && is_nemu_trap) {
    do_nemu_trap()
  }

  // If previous stages have already set some exception bits, they will be considered with higher priorities.
  val is_csr_instr = csr_insn_decoded =/= 0.U && is_csr_hit || system_insn_decoded =/= 0.U || is_nemu_trap
  val is_illegal_instr = in.pipe.exception(i"CAUSE_ILLEGAL_INSTRUCTION") && !is_csr_instr
  out.pipe.exception(i"CAUSE_ILLEGAL_INSTRUCTION") := in_valid && is_illegal_instr
  when(out.pipe.exception(i"CAUSE_ILLEGAL_INSTRUCTION")) {
    out.pipe.tval := (if (p.illegalInstrTval) in.pipe.instr else 0.U)
  }

  def do_fence(): Unit = {}
  def do_pause(): Unit = {}
  def do_ecall(): Unit = {
    UIntToOH(in.state.csr.privilegeMode(1, 0)).asBools.zipWithIndex.foreach { case (v, i) =>
      when(v) {
        out.pipe.exception(i + i"CAUSE_USER_ECALL") := true.B
      }
    }
    out.pipe.tval := 0.U
  }
  def do_ebreak(): Unit = {
    if (p.ebreakAsTrap) {
      do_nemu_trap()
    } else {
      out.pipe.exception(i"CAUSE_BREAKPOINT") := true.B
      out.pipe.tval := 0.U
    }
  }
  def do_fence_i(): Unit = {}
  def do_mret(): Unit = {
    out.pipe.npc := in.state.csr.mepc
    val s = in.state.csr.mstatus
    val prev_prv = get_field(s, i"MSTATUS_MPP")
    val mprv = Mux(prev_prv === u"PRV_M", get_field(s, i"MSTATUS_MPRV"), 0.U)
    val update_fields = Seq(
      ("MSTATUS_MPV", mprv),
      ("MSTATUS_MIE", get_field(s, i"MSTATUS_MPIE")),
      ("MSTATUS_MPIE", 1.U),
      ("MSTATUS_MPP", u"PRV_U"),
      ("MSTATUS_MPV", 0.U),
    )
    val update_s = set_field(s, update_fields)
    out.state.csr.mstatus := update_s
    out.state.csr.privilegeMode := legalize_privilege(prev_prv)
  }
  def do_sret(): Unit = {
    out.pipe.npc := in.state.csr.sepc
    val s = in.state.csr.sstatus
    val prev_prv = get_field(s, i"MSTATUS_SPP")
    val update_fields = Seq(
      ("MSTATUS_SIE", get_field(s, i"MSTATUS_SPIE")),
      ("MSTATUS_SPIE", 1.U),
      ("MSTATUS_SPP", u"PRV_U"),
    )
    out.state.csr.sstatus := set_field(s, update_fields)
    out.state.csr.mstatus := set_field(in.state.csr.mstatus, update_fields :+ ("MSTATUS_MPRV", 0.U))
    out.state.csr.privilegeMode := legalize_privilege(prev_prv)
  }
  def do_wfi(): Unit = {}
  def do_sfence_vma(): Unit = {}
  def do_hfence_vvma(): Unit = {}
  def do_hfence_gvma(): Unit = {}
  def do_nemu_trap(): Unit = {
    out.flags.exit := true.B
  }
}
