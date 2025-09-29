package svm

import chisel3._
import chisel3.util._

abstract class MMU(implicit p: SVMParams) extends PipelineStage with HasMemReadPort {
  override def nMemReadPorts: Int = p.vmLevels

  def effective_priv(uop: MicroOp): UInt

  val satp = in.state.csr.satp
  val priv = effective_priv(in)
  val enable_sv39 = priv <= u"PRV_S" && get_field(satp, "SATP64_MODE") === u"SATP_MODE_SV39"
  val enable_translation = in_valid && in.should_execute && enable_sv39

  val stages = Seq.tabulate(p.vmLevels)(i => Module(new Sv39TranslationStage(effective_priv, i))).reverse

  val ins = io.in +: stages.map(_.io.out)
  val outs = stages.map(_.io.in) :+ io.out
  Seq(ins.zip(outs).head, ins.zip(outs).last).foreach { case (i, o) => o := i }
  // for timing considerations, we add one more pipeline for each intermediate translation stage
  ins.zip(outs).tail.dropRight(1).foreach { case (i, o) => o := PipelineConnect.next(i) }

  val translation_stages = stages.map(_.translation)
  translation_stages.dropRight(1).zip(translation_stages.tail).foreach { case (l, r) =>
    // for timing considerations, we add one more pipeline for each intermediate translation stage
    r.in := PipelineConnect.next(l.out)
  }

  val highest_level = translation_stages.head.in
  highest_level.bits.level.foreach(_ := false.B)
  highest_level.bits.level.last := true.B
  highest_level.bits.pt_base := (satp & u64"SATP64_PPN") << i"RISCV_PGSHIFT"
  highest_level.bits.opcode.foreach(_ := false.B)
  highest_level.bits.miss := false.B

  val lowest_level = translation_stages.last.out
  val lowest_level_valid = io.out.valid && io.out.bits.uop.valid && lowest_level.valid

  stages.map(_.mem).zip(mem_r).foreach { case (m, s) => m <> s }

  def try_force_page_fault(page_fault_hint: MicroOp => Bool, tpe: Int): Unit = {
    val lowest_level_uop = stages.last.io.out.bits.uop.bits
    // DUT-guided forced page fault: when DUT raises exception but REF does not, let REF do it as well.
    when(page_fault_hint(lowest_level_uop) && !lowest_level_uop.pipe.exception(tpe)) {
      out.pipe.exception(tpe) := true.B
      out.pipe.tval := lowest_level.bits.virtual_addr
      printf(p"Force page fault at ${Hexadecimal(lowest_level.bits.virtual_addr)}\n")
    }
  }
}

class IMMU(implicit p: SVMParams) extends MMU {
  override def effective_priv(uop: MicroOp): UInt = uop.state.csr.privilegeMode

  highest_level.valid := enable_translation
  highest_level.bits.virtual_addr := in.state.pc
  highest_level.bits.opcode(2) := true.B // FETCH

  out.pipe.physical_pc := out.state.pc
  when(lowest_level_valid) {
    out.pipe.physical_pc := lowest_level.bits.pt_base
    try_force_page_fault(_.hint.is_instr_page_fault, i"CAUSE_FETCH_PAGE_FAULT")
    out.flags.miss_immu := lowest_level.bits.miss
  }
}

class MMUExpect extends Bundle {
  val paddr = UInt(64.W)
  val s_mode = Bool()
  val sum = Bool()
  val mxr = Bool()
  val opcode = Vec(3, Bool())
}

class DMMU(implicit p: SVMParams) extends MMU with HasMemWriteExpectation[MMUExpect] {
  override def memWriteExpectation: (Int, MMUExpect) = (1, new MMUExpect)
  override def effective_priv(uop: MicroOp): UInt = {
    val mstatus = uop.state.csr.mstatus
    val in_mprv = get_bit(mstatus, "MSTATUS_MPRV")
    Mux(in_mprv, get_field(mstatus, i"MSTATUS_MPP"), uop.state.csr.privilegeMode)
  }

  highest_level.valid := enable_translation && in.pipe.lsu.valid
  highest_level.bits.virtual_addr := in.pipe.lsu.bits.virt_addr
  highest_level.bits.opcode(0) := in.pipe.lsu.bits.opcode(4) // LOAD
  highest_level.bits.opcode(1) := in.pipe.lsu.bits.opcode(3) // STORE

  out.pipe.lsu.bits.phy_addr := out.pipe.lsu.bits.virt_addr
  when(lowest_level_valid) {
    out.pipe.lsu.bits.phy_addr := lowest_level.bits.pt_base
    try_force_page_fault(_.hint.is_load_page_fault, i"CAUSE_LOAD_PAGE_FAULT")
    try_force_page_fault(_.hint.is_store_page_fault, i"CAUSE_STORE_PAGE_FAULT")
    out.flags.miss_dmmu := lowest_level.bits.miss
  }

  val paddr_hint = out.hint.data_paddr
  val mismatch = lowest_level_valid && paddr_hint.valid && lowest_level.bits.pt_base =/= paddr_hint.bits
  val expect = expect_w.head.req
  expect.valid := io.out.valid && io.out.bits.uop.valid && mismatch && !lowest_level.bits.miss
  expect.bits.address := RegEnable(stages.last.mem.address, stages.last.mem.valid)
  val expect_payload = Wire(new MMUExpect)
  expect_payload.paddr := paddr_hint.bits
  expect_payload.s_mode := effective_priv(out) === u"PRV_S"
  expect_payload.sum := get_bit(out.state.csr.sstatus, "MSTATUS_SUM")
  expect_payload.mxr := get_bit(out.state.csr.sstatus, "MSTATUS_MXR")
  expect_payload.opcode := RegEnable(lowest_level.bits.opcode, stages.last.mem.valid)
  expect.bits.payload := expect_payload
  when(expect.valid) {
    out.pipe.lsu.bits.phy_addr := paddr_hint.bits
    out.pipe.exception(i"CAUSE_LOAD_PAGE_FAULT") := false.B
    out.pipe.exception(i"CAUSE_STORE_PAGE_FAULT") := false.B
    Log.ref(p"[${io.out.bits.id}] mmu_expect addr: 0x${Hexadecimal(expect.bits.address)} 0x${Hexadecimal(expect_payload.paddr)}")
  }

  val has_page_fault = out.pipe.exception(i"CAUSE_LOAD_PAGE_FAULT") || out.pipe.exception(i"CAUSE_STORE_PAGE_FAULT")
  when(lowest_level_valid && lowest_level.bits.miss && !has_page_fault) {
    assert(paddr_hint.valid, "DMMU miss must have a valid phy_addr from DUT")
    out.pipe.lsu.bits.phy_addr := paddr_hint.bits
  }

  val expect_store = expect_w.head.resp
  when(expect_store.valid) {
    Log.ref(p"mmu_expect response 0x${Hexadecimal(expect_store.bits.write.wdata.asUInt)}")
    val resp = expect_store.bits
    val pte = new PTEInterpreter(
      resp.write.wdata.asUInt,
      0,
      resp.payload.s_mode,
      resp.payload.sum,
      resp.payload.mxr,
      resp.payload.opcode,
    )
    assert(!pte.is_page_fault && !pte.is_next_level && pte.ppn === resp.payload.paddr >> 12, "expect PTE error")
  }
}

class AddressTranslationInfo(implicit p: SVMParams) extends Bundle {
  val level = Vec(p.vmLevels, Bool())
  val virtual_addr = UInt(64.W)
  val pt_base = UInt(64.W)
  val opcode = Vec(3, Bool()) // LOAD, STORE, FETCH
  val miss = Bool()
}

class Sv39TranslationStage(effective_priv: MicroOp => UInt, i: Int)(implicit p: SVMParams) extends PipelineStage {
  val mem = IO(Flipped(new MemOpReadPortIO))
  val translation = IO(new Bundle {
    val in = Input(Valid(new AddressTranslationInfo))
    val out = Output(Valid(new AddressTranslationInfo))
  })

  PipelineConnect(io.in, io.out)
  PipelineConnect(translation.in, translation.out)
  translation.out.bits.level(i) := false.B

  // page-table entry: read request
  val (idxBits, pgshift) = (9, i"RISCV_PGSHIFT")
  val ptshift = i * idxBits
  val page_index = translation.in.bits.virtual_addr(pgshift + ptshift + idxBits - 1, pgshift + ptshift)
  val pte_paddr = translation.in.bits.pt_base + (page_index << 3)

  mem.valid := in_valid && translation.in.valid && translation.in.bits.level(i)
  mem.address := pte_paddr

  // page-table entry: decode it. See mmu_t::walk in riscv-isa-sim/riscv/mmu.cc
  val opcode = RegEnable(translation.in.bits.opcode, mem.valid)
  val s_mode = effective_priv(out) === u"PRV_S"
  val sum = get_bit(out.state.csr.sstatus, "MSTATUS_SUM")
  val mxr = get_bit(out.state.csr.sstatus, "MSTATUS_MXR")
  val pte = new PTEInterpreter(mem.data.asUInt, i, s_mode, sum, mxr, opcode)

  val pt_base = RegEnable(translation.in.bits.pt_base, mem.valid)
  val pte_addr = RegEnable(mem.address, mem.valid)
  val virt_addr = RegEnable(translation.in.bits.virtual_addr, mem.valid)

  val do_translation = RegNext(mem.valid)
  when(do_translation) {
    Log.ref(
      p"[${io.out.bits.id}] ptw: level $i, vaddr 0x${Hexadecimal(virt_addr)}, " +
        p"pg_base 0x${Hexadecimal(pt_base)}, p_pte 0x${Hexadecimal(pte_addr)}, " +
        p"pte.val 0x${Hexadecimal(pte.value)}"
    )
    when(mem.miss) {
      translation.out.bits.miss := true.B
    }
    if (i > 0) {
      translation.out.bits.level(i - 1) := pte.is_next_level && !mem.miss
    }
    translation.out.bits.pt_base := pte.v2p(virt_addr)
    when(pte.is_page_fault && !mem.miss) {
      val page_fault_type = Seq("LOAD", "STORE", "FETCH")
      opcode.zip(page_fault_type).foreach { case (op, t) =>
        when(op) {
          out.pipe.exception(s"CAUSE_${t}_PAGE_FAULT".macroToInt) := true.B
          out.pipe.tval := virt_addr
        }
      }
    }
  }
}

class PTEInterpreter(pte: UInt, level: Int, s_mode: Bool, sum: Bool, mxr: Bool, opcode: Vec[Bool]) {
  private val (idxBits, pgshift) = (9, i"RISCV_PGSHIFT")
  private val ptshift = level * idxBits
  val ppn = ((pte & (~u"PTE_ATTR").asUInt) >> i"PTE_PPN_SHIFT").asUInt
  private val may_be_next_level = (pte & (u64"PTE_V" | u"PTE_R" | u"PTE_W" | u"PTE_X")) === u64"PTE_V"

  val is_page_fault = WireInit(false.B)
  val is_next_level = WireInit(false.B)

  def value: UInt = pte

  def v2p(virt_addr: UInt): UInt = {
    val next_pt_base = (ppn << pgshift).asUInt
    val vpn = virt_addr >> pgshift
    val page_base = (ppn | vpn(ptshift - 1, 0)) << pgshift
    val superpage_paddr = page_base.asUInt | virt_addr(pgshift - 1, 0)
    Mux(may_be_next_level, next_pt_base, superpage_paddr)
  }

  when(get_field(pte, "PTE_RSVD") =/= 0.U) {
    is_page_fault := true.B
  }.elsewhen(get_bit(pte, "PTE_N")) {
    is_page_fault := true.B
  }.elsewhen(get_field(pte, "PTE_PBMT") =/= 0.U) {
    is_page_fault := true.B
  }.elsewhen(may_be_next_level) { // next level of page table
    when((pte & (u64"PTE_D" | u"PTE_A" | u"PTE_U" | u"PTE_N" | u"PTE_PBMT")) =/= 0.U) {
      is_page_fault := true.B
    }.otherwise {
      is_next_level := true.B
    }
  }.elsewhen(Mux(get_bit(pte, "PTE_U"), s_mode && (opcode(2) || !sum), !s_mode)) {
    is_page_fault := true.B
  }.elsewhen(!get_bit(pte, "PTE_V") || (!get_bit(pte, "PTE_R") && get_bit(pte, "PTE_W"))) {
    is_page_fault := true.B
  }.elsewhen(
    (opcode(2) && !get_bit(pte, "PTE_X")) ||
      (opcode(1) && !(get_bit(pte, "PTE_R") && get_bit(pte, "PTE_W"))) ||
      (opcode(0) && !get_bit(pte, "PTE_R") && !(mxr && get_bit(pte, "PTE_X")))
  ) {
    is_page_fault := true.B
  }.elsewhen(ppn(ptshift - 1, 0) =/= 0.U) {
    is_page_fault := true.B
  }.otherwise {
    val ad = u64"PTE_A" | Mux(opcode(1), u64"PTE_D", 0.U)
    // Svadu and Svnapot not supported
    when((pte & ad) =/= ad || get_bit(pte, "PTE_N")) {
      is_page_fault := true.B
    }
  }
}
