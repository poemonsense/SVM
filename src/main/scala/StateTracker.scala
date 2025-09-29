package svm

import chisel3._
import chisel3.util._
import difftest.UARTIO
import difftest.common.LogPerfControl
import svm.util.VerificationExtractor

class StateTracker(stepT: GoldenStep)(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val rw = new RefBusIO
    val step = Input(Valid(stepT))
    val execute = ValidIO(new PipelineBits)
    val commit = Flipped(ValidIO(new PipelineBits))
    val dut_reset = Output(Bool())
    val core_reset = Output(Bool())
    val dut_exit = Input(UInt(64.W))
    val allow_speculation = Output(Bool())
    val state = Output(UInt(64.W))
    val uart = new UARTIO
  })

  val m_standalone_reset :: m_standalone :: m_ref :: m_ref_reset :: Nil = Enum(4)
  val current_mode = RegInit(m_standalone_reset)

  val standalone_tracker = Module(new SingleStepTracker)
  val standalone_reset_vector = RegInit((p.withSRAM._1 + 0x20).U(64.W))
  standalone_tracker.reset := reset.asBool || current_mode =/= m_standalone
  standalone_tracker.io.reset_vector.valid := current_mode === m_standalone_reset || current_mode === m_ref_reset
  standalone_tracker.io.reset_vector.bits := standalone_reset_vector
  standalone_tracker.io.valid := current_mode === m_standalone
  standalone_tracker.io.commit := io.commit

  val ref_tracker = Module(new RefStateTracker(stepT))
  ref_tracker.reset := reset.asBool || current_mode =/= m_ref
  ref_tracker.io.step := io.step
  ref_tracker.io.commit := io.commit

  val state_recorder = Module(new StateRecorder)
  state_recorder.io.rw <> io.rw
  state_recorder.io.ref_state := Mux(current_mode === m_ref, ref_tracker.io.state, 0.U)
  state_recorder.io.dut_exit := Mux(current_mode === m_ref, io.dut_exit, 0.U)
  state_recorder.io.uart <> io.uart

  val dut_reset_counter = RegInit(0.U(8.W))
  io.execute := Mux(current_mode === m_standalone, standalone_tracker.io.execute, ref_tracker.io.execute)
  when(current_mode === m_standalone) {
    ref_tracker.io.step.valid := false.B
    ref_tracker.io.commit.valid := false.B
    when(standalone_tracker.io.exit) {
      current_mode := m_ref_reset
    }
  }.elsewhen(current_mode === m_ref) {
    standalone_tracker.io.commit.valid := false.B
    // exit m_ref when errors
    val exit_ref_mode = io.dut_exit =/= 0.U || ref_tracker.io.state =/= 0.U || state_recorder.io.assertion_triggered
    when(exit_ref_mode) {
      current_mode := m_standalone_reset
      standalone_reset_vector := p.withSRAM._1.U
    }
  }.otherwise {
    dut_reset_counter := dut_reset_counter + 1.U
    when(dut_reset_counter.andR) {
      when(current_mode === m_ref_reset) {
        current_mode := m_ref
      }.otherwise {
        current_mode := m_standalone
      }
    }
  }

  io.dut_reset := current_mode =/= m_ref
  io.core_reset := current_mode === m_standalone_reset || current_mode === m_ref_reset
  io.allow_speculation := current_mode === m_ref
  io.state := state_recorder.io.state
}

class SingleStepTracker(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val reset_vector = Input(Valid(UInt(64.W)))
    val exit = Output(Bool())
    val execute = ValidIO(new PipelineBits)
    val commit = Flipped(ValidIO(new PipelineBits))
  })

  val arch_state = Reg(new ArchState)
  when(io.reset_vector.valid) {
    arch_state.pc := io.reset_vector.bits
    arch_state.xpr.value.foreach(_ := 0.U)
    arch_state.csr.privilegeMode := 0.U
    arch_state.csr.privilegeMode := 0.U
    arch_state.csr.mstatus := "ha00001800".U
    arch_state.csr.sstatus := "ha00001800".U
    arch_state.csr.satp := 0.U
    arch_state.csr.mip := 0.U
    arch_state.csr.mie := 0.U
    arch_state.csr.mideleg := 0.U
    arch_state.csr.medeleg := 0.U
  }

  val s_idle :: s_issue :: s_execute :: s_exit :: Nil = Enum(4)
  val state = RegInit(s_idle)

  when(state === s_idle) {
    when(io.valid) {
      state := s_issue
    }
  }.elsewhen(state === s_issue) {
    when(io.execute.valid) {
      state := s_execute
    }
  }.elsewhen(state === s_execute) {
    when(io.commit.valid) {
      when(io.commit.bits.uop.bits.state.pc === p.memBase.U) {
        state := s_exit
      }.otherwise {
        state := s_issue
        arch_state := io.commit.bits.uop.bits.state
      }
    }
  }

  io.execute.valid := state === s_issue
  io.execute.bits.id := 0.U
  io.execute.bits.uop.valid := state === s_issue
  io.execute.bits.uop.bits.flags.miss_immu := false.B
  io.execute.bits.uop.bits.flags.miss_fetch := false.B
  io.execute.bits.uop.bits.flags.miss_load := false.B
  io.execute.bits.uop.bits.flags.miss_dmmu := false.B
  io.execute.bits.uop.bits.flags.exit := false.B
  io.execute.bits.uop.bits.state := arch_state
  io.execute.bits.uop.bits.hint := DontCare
  io.execute.bits.uop.bits.pipe := DontCare
  io.execute.bits.more.foreach(_.valid := false.B)
  io.execute.bits.more.head.valid := true.B
  io.execute.bits.more.foreach(_.bits := 0.U.asTypeOf(new ExecutionHint))

  io.exit := state === s_exit
}

class RefStateTracker(stepT: GoldenStep)(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val step = Input(Valid(stepT))
    val execute = ValidIO(new PipelineBits)
    val commit = Flipped(ValidIO(new PipelineBits))
    val state = Output(UInt(64.W))
  })

  val has_uop = VecInit(io.step.bits.uop.map(_.valid)).asUInt.orR
  assert(!io.step.valid || has_uop, "step not for instr not supported")

  // States
  val nEntries = p.nInflight
  val s_valid = RegInit(VecInit.fill(nEntries)(false.B))
  val stateSRAM = SyncReadMem(nEntries, new ArchState)
  assert(!s_valid.asUInt.andR || !io.step.valid, "step valid but state entries are full")

  // Allocation for in-coming instructions
  val allocate_id = PriorityEncoder(s_valid.map(!_))
  val allocate_state = Wire(new ArchState)
  allocate_state.pc := PriorityMux(io.step.bits.uop.map(_.valid), io.step.bits.uop.map(_.bits.pc))
  allocate_state.xpr := io.step.bits.regs_int
  allocate_state.fpr := io.step.bits.regs_fp
  allocate_state.csr := io.step.bits.csr
  when(io.step.valid) {
    stateSRAM.write(allocate_id, allocate_state)
    s_valid(allocate_id) := true.B
    for (commit <- io.step.bits.uop) {
      when(commit.valid) {
        Log.dut(p"[$allocate_id] commit ${commit.bits}")
      }
    }
  }

  // First instruction checker logic
  val is_first_step = firstTimeValid(io.step.valid)
  val state_r = RegEnable(allocate_state, io.step.valid)
  val state_base = Mux(is_first_step, allocate_state, state_r)
  io.execute.valid := io.step.valid
  io.execute.bits.id := allocate_id
  io.execute.bits.uop.valid := DontCare
  io.execute.bits.uop.bits.flags.miss_immu := false.B
  io.execute.bits.uop.bits.flags.miss_fetch := false.B
  io.execute.bits.uop.bits.flags.miss_load := false.B
  io.execute.bits.uop.bits.flags.miss_dmmu := false.B
  io.execute.bits.uop.bits.flags.exit := false.B
  io.execute.bits.uop.bits.state := state_base
  io.execute.bits.uop.bits.state.pc := allocate_state.pc
  io.execute.bits.uop.bits.hint := DontCare
  io.execute.bits.uop.bits.pipe := DontCare
  io.step.bits.uop.zip(io.execute.bits.more).foreach { case (in, out) =>
    out.valid := in.valid
    out.bits.skip.valid := in.bits.is_skip
    out.bits.skip.bits.wen := in.bits.rfwen
    out.bits.skip.bits.dest := in.bits.instr.rd
    out.bits.skip.bits.data := in.bits.rd_d
    out.bits.interrupt := in.bits.is_interrupt
    out.bits.is_instr_page_fault := in.bits.is_instr_page_fault
    out.bits.is_load_page_fault := in.bits.is_load_page_fault
    out.bits.is_store_page_fault := in.bits.is_store_page_fault
    out.bits.div_data := in.bits.div_data
    out.bits.data_paddr.valid := in.bits.data_paddr.valid
    // TODO: use data_paddr for fflags is only a temp fix
    out.bits.data_paddr.bits := Mux(in.bits.data_paddr.valid, in.bits.data_paddr.bits, allocate_state.csr.fcsr)
    out.bits.is_rvc := in.bits.is_rvc
    out.bits.instr := in.bits.inst_val
  }

  // commit results
  val commit_valid = io.commit.valid
  val dut_commit_state = stateSRAM.read(io.commit.bits.id, commit_valid)
  val ref_commit_valid = RegNext(commit_valid, false.B)
  val ref_commit_uop = RegEnable(io.commit.bits.uop.bits, commit_valid)
  val ref_commit_exit = ref_commit_uop.flags.exit
  val ref_commit_state = ref_commit_uop.state
  val check_commit_state = ref_commit_valid && !ref_commit_exit
  val ref_pc = Reg(chiselTypeOf(ref_commit_state.pc))
  when(is_first_step) {
    ref_pc := allocate_state.pc
  }.elsewhen(ref_commit_valid) {
    ref_pc := ref_commit_state.pc
  }
  val checker_regs_int = ResultChecker(
    right = ref_commit_state.xpr.toSeq,
    wrong = dut_commit_state.xpr.toSeq,
    cond = check_commit_state,
    target = ref_commit_state.xpr.names,
    pc = Some(ref_pc),
  )
  val checker_csr = ResultChecker(
    right = ref_commit_state.csr.toSeq,
    wrong = dut_commit_state.csr.toSeq,
    cond = check_commit_state,
    target = ref_commit_state.csr.names,
    pc = Some(ref_pc),
  )
  val checker_pc = ResultChecker(
    right = ref_pc,
    wrong = dut_commit_state.pc,
    cond = check_commit_state,
    target = "pc",
    pc = Some(ref_pc),
  )
  val checker_regs_fp = ResultChecker(
    right = ref_commit_state.fpr.toSeq,
    wrong = dut_commit_state.fpr.toSeq,
    cond = check_commit_state,
    target = ref_commit_state.fpr.names,
    pc = Some(ref_pc),
  )
  val result_compare_failed = check_commit_state && (checker_regs_int || checker_csr || checker_pc || checker_regs_fp)

  val check_commit_id = RegEnable(io.commit.bits.id, commit_valid)
  when(ref_commit_valid) {
    s_valid(check_commit_id) := false.B
    when(result_compare_failed) {
      Log.cond("========== REF Regs ==========")
      ResultChecker.display(ref_commit_state.xpr.names, ref_commit_state.xpr.toSeq)
      ResultChecker.display(ref_commit_state.fpr.names, ref_commit_state.fpr.toSeq)
      ResultChecker.display(ref_commit_state.csr.names, ref_commit_state.csr.toSeq)
      ResultChecker.display(Seq("pc"), Seq(ref_pc))
      Log.cond(p"[$check_commit_id] commit $ref_commit_uop BUG")
    }
  }
  val is_trap = ref_commit_valid && ref_commit_exit
  val is_error = check_commit_state && result_compare_failed
  io.state := Cat(is_error, is_trap)
}

class RefBusIO extends Bundle {
  val read = Input(Bool())
  val write = Input(Bool())
  val addr = Input(UInt(16.W))
  val rdata = Output(UInt(64.W))
  val wdata = Input(UInt(64.W))
}

class StateRecorder extends Module {
  val io = IO(new Bundle {
    val rw = new RefBusIO
    val assertion_triggered = Output(Bool())
    val ref_state = Input(UInt(64.W))
    val dut_exit = Input(UInt(64.W))
    val state = Output(UInt(64.W))
    val uart = new UARTIO
  })

  val ref_state_reg = RegInit(0.U(64.W))
  when(io.ref_state =/= 0.U) {
    ref_state_reg := ref_state_reg | io.ref_state
  }

  val dut_state_reg = RegInit(0.U(64.W))
  when(io.dut_exit =/= 0.U) {
    dut_state_reg := io.dut_exit
  }

  val system_state_reg = RegInit(0.U(64.W))
  io.state := system_state_reg

  val uart_tx_reg = RegInit(0x100.U(64.W))
  uart_tx_reg := 0x100.U
  io.uart.in.valid := false.B
  io.uart.out.valid := !uart_tx_reg(8)
  io.uart.out.ch := uart_tx_reg(7, 0)

  val uart_hex_out_valid = RegInit(false.B)
  val uart_hex_out_reg = RegInit(0.U(64.W))
  val uart_hex_out_index = RegInit(0.U(64.W))
  def uart_hex_out_write(wdata: UInt): Unit = {
    uart_hex_out_valid := true.B
    uart_hex_out_reg := wdata
    when(uart_hex_out_index === 0.U) {
      uart_hex_out_index := 16.U
    }
  }

  when(uart_hex_out_valid) {
    uart_hex_out_index := uart_hex_out_index - 1.U
    when(uart_hex_out_index === 1.U) {
      uart_hex_out_valid := false.B
    }.otherwise {
      uart_hex_out_reg := Cat(uart_hex_out_reg(59, 0), 0.U(4.W))
    }
    io.uart.out.valid := true.B
    val ch = uart_hex_out_reg(63, 60)
    io.uart.out.ch := Mux(ch >= 0xa.U, ('a' - 0xa).U + ch, '0'.toInt.U + ch)
  }

  val rw_regs = Seq(
    ref_state_reg, dut_state_reg, system_state_reg, uart_tx_reg, uart_hex_out_reg, uart_hex_out_index,
  ) ++ Seq.fill(10)(RegInit(0.U(64.W)))

  // Assertion states
  val assertion_reg = Seq.fill(16)(RegInit(0.U(64.W)))
  io.assertion_triggered := VecInit(assertion_reg.map(_.orR)).asUInt.orR

  // assertions are implicitly wired from sources to here by transforms
  val assertion = Seq.fill(16 * 64)(WireInit(false.B))
  VerificationExtractor.sink(assertion)
  for ((w, r) <- assertion.grouped(64).zip(assertion_reg)) {
    val u = VecInit(w).asUInt
    when(u.orR) {
      r := r | u
    }
  }

  // Performance counters
  val n_perf_counter = 96
  val perf_counter_reg = Seq.fill(n_perf_counter)(RegInit(0.U(64.W)))
  val inc = Seq.fill(n_perf_counter)(WireInit(0.U(64.W)))
  inc.foreach(PerfCounter.sink)
  for ((i, p) <- inc.zip(perf_counter_reg)) {
    p := p + i
  }

  // MMIO read/write
  val memory_mapped_registers = rw_regs ++ assertion_reg ++ perf_counter_reg

  val raddr = RegEnable(io.rw.addr(log2Ceil(memory_mapped_registers.length) - 1, 0), io.rw.read)
  io.rw.rdata := VecInit(memory_mapped_registers)(raddr)

  when(io.rw.write) {
    for ((r, i) <- memory_mapped_registers.zipWithIndex) {
      when(io.rw.addr === i.U) {
        if (i == 4) {
          uart_hex_out_write(io.rw.wdata)
        } else {
          r := io.rw.wdata
        }
      }
    }
  }
}

object Log {
  def apply(p: Printable)(implicit params: SVMParams): Unit = {
    if (params.enableDebug) {
      val ctrl = LogPerfControl()
      cond(p"[time=${ctrl.timer(31, 0)}] $p", ctrl.logEnable)
    }
  }

  def cond(p: Printable, cond: Bool = true.B)(implicit params: SVMParams): Unit = {
    if (params.enableDebug) {
      when(cond) {
        printf(p + "\n")
      }
    }
  }

  def ref(p: Printable)(implicit params: SVMParams): Unit = apply(p"[REF] " + p)
  def dut(p: Printable)(implicit params: SVMParams): Unit = apply(p"[DUT] " + p)
}

object ResultChecker {
  def display(reg_names: Seq[String], regs: Seq[UInt])(implicit p: SVMParams): Unit = {
    val max_len = reg_names.map(_.length).max
    val group_size = if (max_len <= 4) 4 else 3
    reg_names
      .zip(regs)
      .grouped(group_size)
      .foreach(group => {
        Log.cond(group.map { case (n, v) => p"" + f"$n%4s: " + p"0x${Hexadecimal(v)}" }.reduce(_ + " " + _))
      })
  }

  def apply(right: UInt, wrong: UInt, cond: Bool, target: String, pc: Option[UInt])(implicit p: SVMParams): Bool = {
    val checker = cond && right =/= wrong
    when(checker) {
      val pc_msg = if (pc.isDefined) p" at pc = 0x${Hexadecimal(pc.get)}" else p""
      Log.cond(p"$target different$pc_msg, right = 0x${Hexadecimal(right)}, wrong = 0x${Hexadecimal(wrong)}")
    }
    checker
  }

  def apply(right: Seq[UInt], wrong: Seq[UInt], cond: Bool, target: Seq[String], pc: Option[UInt])(implicit
    p: SVMParams
  ): Bool = {
    VecInit(right.zip(wrong).zip(target).map { case ((r, w), t) =>
      apply(r, w, cond, t, pc)
    }).asUInt.orR
  }

  def apply(right: Seq[UInt], wrong: Seq[UInt], cond: Bool, target: String, pc: Option[UInt])(implicit
    p: SVMParams
  ): Bool = {
    apply(right, wrong, cond, Seq.tabulate(right.length)(i => s"$target$i"), pc)
  }
}
