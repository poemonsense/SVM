package svm

import chisel3._
import chisel3.util._
import difftest.bist._
import difftest._

class Endpoint(implicit p: SVMParams) extends Module {
  val bistP = p.toBISTParams
  val io = IO(new BISTEndpointIO(bistP))

  val interfaces = RegNext(RegNext(io.dut.interfaces, 0.U.asTypeOf(io.dut.interfaces)), 0.U.asTypeOf(io.dut.interfaces))
  val diffState = RegInit(0.U.asTypeOf(new DiffState(p.hartId)(bistP)))
  diffState.update(interfaces.asTypeOf(MixedVec(p.bundleTypes)).zip(p.bundleTypes.map(_.desiredCppName)))

  val svm = Module(new SVM)

  io.dut.reset := svm.dut.reset

  svm.dut.step.valid := diffState.is_valid
  for (i <- 0 until p.nCommits) {
    val uop = svm.dut.step.bits.uop(i)
    uop.valid := diffState.commit(i).valid
    uop.bits.fromInstrCommit(diffState.commit(i))
    uop.bits.is_interrupt := 0.U
    uop.bits.is_instr_page_fault := false.B
    uop.bits.is_load_page_fault := false.B
    uop.bits.is_store_page_fault := false.B
    uop.bits.is_trap := diffState.trap.hasTrap
    uop.bits.rd_d := diffState.rd_d(i)
    uop.bits.div_data := diffState.guidance(i).div
    uop.bits.data_paddr := diffState.guidance(i).data_paddr
  }
  // When exception occurs, set the instruction commit
  when(diffState.event.valid && (diffState.event.exception =/= 0.U || diffState.event.interrupt =/= 0.U)) {
    val uop = svm.dut.step.bits.uop.head
    uop.valid := true.B
    uop.bits.pc := diffState.event.exceptionPC
    uop.bits.inst_val := diffState.event.exceptionInst
    uop.bits.is_interrupt := diffState.event.interrupt
    uop.bits.is_instr_page_fault := diffState.event.exception === 12.U
    uop.bits.is_load_page_fault := diffState.event.exception === 13.U
    uop.bits.is_store_page_fault := diffState.event.exception === 15.U
    uop.bits.is_trap := false.B
    uop.bits.data_paddr.valid := false.B
  }
  svm.dut.step.bits.regs_int := diffState.regs_int
  svm.dut.step.bits.csr := diffState.csr
  svm.dut.step.bits.regs_fp := DontCare
  diffState.regs_fp.foreach(r => svm.dut.step.bits.regs_fp := r)

  val mem = RegNext(RegNext(io.dut.mem, 0.U.asTypeOf(io.dut.mem)), 0.U.asTypeOf(io.dut.mem))
  svm.dut.mem := io.dut.mem

  io.ref_ := svm.ref
}

class DiffState(coreid: Int)(implicit p: BISTParams) extends Bundle {
  private def extract_valid(data: Data): Seq[Bool] = {
    data match {
      case c: InstrCommit        => Seq(c.valid && !c.isDelayedWb)
      case b: DifftestBaseBundle => Option.when(b.hasValid)(b.getValid).toSeq
      case v: Vec[_]             => v.flatMap(extract_valid)
      case x =>
        println(s"Unsupported type: ${x.getClass.getName}")
        Seq()
    }
  }
  def is_valid: Bool = VecInit(getElements.flatMap(extract_valid)).asUInt.orR

  def update(in: Seq[(DifftestBundle, String)]): DiffState = {
    def filterInputs[T <: DifftestBundle](name: String): Seq[T] = {
      in.filter(_._2 == name).map(_._1).asInstanceOf[Seq[T]]
    }
    this
      .updateRegsInt(filterInputs[DiffArchIntRegState]("regs_int"))
      .updateCSR(filterInputs[DiffCSRState]("csr"), filterInputs[DiffExtraCSRState]("csr_extra"))
      .updateEvent(filterInputs[DiffArchEvent]("event"))
      .updateCommit(filterInputs[DiffInstrCommit]("commit"), filterInputs[DiffGuidanceInfo]("guidance"))
      .updateTrap(filterInputs[DiffTrapEvent]("trap"))
      .updateWbInt(filterInputs[DiffIntWriteback]("wb_int"))
      .updateRegsFp(filterInputs[DiffArchFpRegState]("regs_fp"))
      .updateWbFp(filterInputs[DiffFpWriteback]("wb_fp"))
      .updateDelayedIntWb(filterInputs[DiffArchIntDelayedUpdate]("regs_int_delayed"))
      .updateDelayedFpWb(filterInputs[DiffArchFpDelayedUpdate]("regs_fp_delayed"))
  }

  private def update[T <: DifftestBaseBundle, TT <: DifftestBundle](
    in: Seq[TT],
    state: T,
    index: Int = 0,
  ): DiffState = {
    state.defaultUpdate()
    in.foreach(i => {
      when(i.coreid === coreid.U && i.getIndex.getOrElse(0.U) === index.U && i.bits.needUpdate.getOrElse(true.B)) {
        state.update(i.bits)
      }
    })
    this
  }

  // DiffArchIntRegState
  val regs_int: ArchIntRegState = new ArchIntRegState
  def updateRegsInt(in: Seq[DiffArchIntRegState]): DiffState = update(in, regs_int)

  // DiffIntWriteback
  val wb_int: DataWritebackFlatten = new DataWritebackFlatten(p.nPhyRegs)
  def updateWbInt(in: Seq[DiffIntWriteback]): DiffState = update(in, wb_int)
  def updateDelayedIntWb(in: Seq[DiffArchIntDelayedUpdate]): DiffState = {
    commit.foreach(c =>
      in.foreach(i => {
        when(c.valid && c.isDelayedWb && c.rfwen && i.valid && !i.nack && i.address === c.wdest) {
          wb_int.data(c.wpdest) := i.data
          c.setSpecial(isDelayedWb = false.B, isExit = c.isExit)
        }
      })
    )
    this
  }

  // DiffCSRState
  val csr: FullCSRState = new FullCSRState
  def updateCSR(in: Seq[DiffCSRState], extra: Seq[DiffExtraCSRState]): DiffState = {
    update(in, csr).update(extra, csr)
  }

  // DiffArchEvent
  val event: ArchEvent = new ArchEvent
  def updateEvent(in: Seq[DiffArchEvent]): DiffState = update(in, event)

  // DiffInstrCommit
  val commit: Vec[InstrCommit] = Vec(p.nCommits, new InstrCommit(p.nPhyRegs))
  val guidance: Vec[GuidanceInfo] = Vec(p.nCommits, new GuidanceInfo)
  def updateCommit(in: Seq[DiffInstrCommit], guidance_in: Seq[DiffGuidanceInfo]): DiffState = {
    (0 until p.nCommits).foldLeft(this) { case (this_p, i) =>
      this_p.update(in, commit(i), i).update(guidance_in, guidance(i), i)
    }
  }

  // DiffTrapEvent
  val trap: TrapEvent = new TrapEvent
  def updateTrap(in: Seq[DiffTrapEvent]): DiffState = update(in, trap)

  // DiffArchFpRegState
  val regs_fp: Option[ArchFpRegState] = Option.when(p.hasFpExtension)(new ArchFpRegState)
  def updateRegsFp(in: Seq[DiffArchFpRegState]): DiffState = regs_fp.map(r => update(in, r)).getOrElse(this)

  // DiffFpWriteback
  val wb_fp: Option[DataWritebackFlatten] = Option.when(p.hasFpExtension)(new DataWritebackFlatten(p.nPhyRegs))
  def updateWbFp(in: Seq[DiffFpWriteback]): DiffState = wb_fp.map(w => update(in, w)).getOrElse(this)
  def updateDelayedFpWb(in: Seq[DiffArchFpDelayedUpdate]): DiffState = {
    commit.foreach(c =>
      in.foreach(i => {
        when(c.valid && c.isDelayedWb && c.fpwen && i.valid && !i.nack && i.address === c.wdest) {
          wb_fp.foreach(_.data(c.wpdest) := i.data)
          c.setSpecial(isDelayedWb = false.B, isExit = c.isExit)
        }
      })
    )
    this
  }

  // rd_d: from `wb_int` instead of `regs_int` here to avoid WAW and capture writes to $zero
  def rd_d(i: Int): UInt = {
    val xpr = wb_int.data(commit(i).wpdest)
    val fpr = wb_fp.map(_.data(commit(i).wpdest))
    fpr.map(f => Mux(commit(i).fpwen, f, xpr)).getOrElse(xpr)
  }
}
