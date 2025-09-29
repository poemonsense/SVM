package svm

import chisel3._
import chisel3.util._
import svm.definitions.{Instruction, MstatusCSRDef, RVInstruction, RVInstructions, StateTransition, When}

class Arithmetic(implicit p: SVMParams) extends PipelineStage {
  val s0 = WireInit(io.in)
  val s0_uop = s0.bits.uop.bits
  val s1 = PipelineConnect.next(s0)
  io.out := s1

  val instr = new RVInstruction(in.pipe.instr)
  val supported_insns = p.all_insn.filterNot(RVInstructions.system.contains)
  val decoded = Decoder(in.pipe.instr, supported_insns)
  val instr_ops = supported_insns.map(Instruction.fromSpike)
  val state = in.state

  // PC update and instruction execution
  val do_skip = in_valid && in.hint.skip.valid
  val do_execution = in_valid && in.should_execute

  val (redirectValid, redirectPc) = extractStateTransition("npc").collect {
    case StateTransition(_, Some(cond), Some(value), _) => (cond.B(in), value.U(in))
  }.unzip
  val redirectTaken = do_execution && VecInit(redirectValid).asUInt.orR
  val snpc = SignExt(in.state.pc + Mux(in.pipe.is_rvc, 2.U, 4.U), 64)
  when(in_valid && !in.has_trap) {
    s0_uop.pipe.npc := Mux(redirectTaken, Mux1H(redirectValid, redirectPc), snpc)
  }

  val (xprWriteValid, xprWriteVal) = extractStateTransition("xpr").collect {
    case StateTransition(_, Some(cond), Some(value), _) => (RegNext(cond.B(in)), RegNext(value.U(in)))
  }.unzip
  out.pipe.xpr.valid := RegNext(do_execution, false.B) && VecInit(xprWriteValid).asUInt.orR
  out.pipe.xpr.bits.dest := new RVInstruction(s1.bits.uop.bits.pipe.instr).rd
  out.pipe.xpr.bits.data := Mux1H(xprWriteValid, xprWriteVal)
  when(RegNext(do_skip && in.hint.skip.bits.wen, false.B)) {
    out.pipe.xpr.valid := true.B
    out.pipe.xpr.bits.dest := s1.bits.uop.bits.hint.skip.bits.dest
    out.pipe.xpr.bits.data := s1.bits.uop.bits.hint.skip.bits.data
  }

  s0_uop.pipe.fpr.valid := false.B
  s0_uop.pipe.fpr.bits := DontCare
  val (fprWriteValid, fprWriteVal) = extractStateTransition("fpr").collect {
    case StateTransition(_, Some(cond), Some(value), _) => (cond.B(in), value.U(in))
  }.unzip
  if (fprWriteValid.nonEmpty) {
    s0_uop.pipe.fpr.valid := do_execution && VecInit(fprWriteValid).asUInt.orR
    s0_uop.pipe.fpr.bits.dest := instr.rd
    s0_uop.pipe.fpr.bits.data := Mux1H(fprWriteValid, fprWriteVal)
  }

  extractStateTransition("assert").collect { case StateTransition(_, Some(cond), _, Some(msg)) =>
    assert(!do_execution || cond.B(in), msg)
  }

  // TODO: implement hardfloat and use real fflags here
  val fflags_cond = extractStateTransition("fflags").collect { case StateTransition(_, Some(cond), _, _) => cond.B(in) }
  if (fflags_cond.nonEmpty) {
    when(do_execution && VecInit(fflags_cond).asUInt.orR) {
      s0_uop.state.csr.fcsr := in.hint.data_paddr.bits
      val mstatusCsrDef = new MstatusCSRDef(None, None)
      s0_uop.state.csr.mstatus := mstatusCsrDef.adjust_sd(set_field(in.state.csr.mstatus, i"MSTATUS_FS", 3.U))
    }
  }

  // Set the illegal instruction exception bit if applicable
  when(do_execution && !in.pipe.exception.asUInt.orR) {
    s0_uop.pipe.exception(i"CAUSE_ILLEGAL_INSTRUCTION") := !decoded
  }

  def extractStateTransition(implicit target: String): Seq[StateTransition] = {
    decoded.asBools.zip(instr_ops.map(extractStateTransition)).flatMap { case (v, ops) => ops.map(_.when(v)) }
  }

  def extractStateTransition(i: Instruction)(implicit target: String): Iterable[StateTransition] = {
    i.flatten.flatMap {
      case st: StateTransition => Option.when(target == st.tpe)(st)
      case When(cond, thenp, elsep) =>
        val (thens, elses) = (extractStateTransition(thenp), extractStateTransition(elsep))
        thens.map(_.when(cond)) ++ elses.map(_.when(cond.not))
      case _ => None
    }
  }
}
