package svm

import chisel3._
import chisel3.util._
import svm.definitions.{AMO, Instruction, Load, RVInstructions, Store}

object LSUOpcode {
  def apply(): UInt = UInt(6.W)

  def apply(is_lrsc: Bool, is_load: Bool, is_store: Bool, is_unsigned: Bool, req_size: UInt): UInt = {
    Cat(is_lrsc, is_load, is_store, is_unsigned, ZeroExt(req_size, 2))
  }
}

abstract class LSU(implicit p: SVMParams) extends PipelineStage {
  protected def lsu_valid(pipe: Valid[PipelineBits]): Bool = {
    pipe.valid && pipe.bits.uop.valid && pipe.bits.uop.bits.should_execute && pipe.bits.uop.bits.pipe.lsu.valid
  }

  protected def byte_offset(address: UInt): UInt = Cat(address(2, 0), 0.U(3.W))

  protected def do_align_wdata(data: UInt, address: UInt): UInt = (data << byte_offset(address))(63, 0)

  protected def do_align_rdata(data: UInt, address: UInt): UInt = (data >> byte_offset(address))(63, 0)

  protected def do_extend(data: UInt, opcode: UInt): UInt = {
    MuxLookup(opcode(2, 0), data(63, 0))(
      Seq(
        "b000".U -> SignExt(data(7, 0), 64),
        "b001".U -> SignExt(data(15, 0), 64),
        "b010".U -> SignExt(data(31, 0), 64),
        "b100".U -> ZeroExt(data(7, 0), 64),
        "b101".U -> ZeroExt(data(15, 0), 64),
        "b110".U -> ZeroExt(data(31, 0), 64),
      )
    )
  }

  protected def store_mask(address: UInt, opcode: UInt): UInt = {
    (MuxLookup(opcode(1, 0), "b11111111".U(8.W))(
      Seq(
        "b00".U -> "b1".U(8.W),
        "b01".U -> "b11".U(8.W),
        "b10".U -> "b1111".U(8.W),
      )
    ) << address(2, 0))(7, 0)
  }
}

class LoadUnit(implicit p: SVMParams) extends LSU with HasMemReadPort with HasMemWriteExpectation[MemOpWriteData] {
  override def nMemReadPorts: Int = 1
  override def memWriteExpectation: (Int, MemOpWriteData) = (1, new MemOpWriteData)

  val allow_speculation = IO(Input(Bool()))

  val pipe = PipelineConnect.next(io.in)
  io.out := pipe

  // Real read data
  val read = mem_r.head
  read.valid := lsu_valid(io.in) && in.pipe.lsu.bits.opcode(4)
  read.address := in.pipe.lsu.bits.phy_addr

  val load_enable_reg = RegNext(read.valid, false.B)
  val load_addr = out.pipe.lsu.bits.phy_addr
  val load_opcode = out.pipe.lsu.bits.opcode
  val load_data = do_extend(do_align_rdata(read.data.asUInt, load_addr), load_opcode)
  when(load_enable_reg) {
    Log.ref(p"[${io.out.bits.id}] mem_load  addr: 0x${Hexadecimal(load_addr)}, data: 0x${Hexadecimal(load_data)}")
  }

  // We speculatively use the load data from DUT because this address may be updated
  // by another earlier instruction still in the pipelines.
  val dut_load_data = pipe.bits.uop.bits.hint.load_data
  out.pipe.lsu.bits.load_data := Mux(allow_speculation, dut_load_data, load_data)
  out.flags.miss_load := load_enable_reg && read.miss

  val expect = expect_w.head.req
  val load_difference = load_data =/= dut_load_data && out.pipe.xpr.bits.dest =/= 0.U
  expect.valid := load_enable_reg && allow_speculation && load_difference && !read.miss
  expect.bits.address := load_addr
  val payload = expect.bits.payload
  payload.wdata := do_align_wdata(do_extend(dut_load_data, load_opcode), load_addr).asTypeOf(payload.wdata)
  val expect_bytes = dut_load_data.asTypeOf(Vec(8, UInt(8.W)))
  val current_bytes = load_data.asTypeOf(Vec(8, UInt(8.W)))
  val expect_mask = VecInit(expect_bytes.zip(current_bytes).map(x => x._1 =/= x._2)).asUInt
  val expect_mask_sel = VecInit.tabulate(4)(i => ZeroExt(expect_mask((1 << i) - 1, 0), 8))(load_opcode(1, 0))
  payload.wmask := (expect_mask_sel << load_addr(2, 0)).asTypeOf(payload.wmask)
  when(expect.valid) {
    Log.ref(
      p"[${io.out.bits.id}] mem_expect addr: 0x${Hexadecimal(load_addr)}, " +
        p"data: 0x${Hexadecimal(payload.wdata.asUInt)}, mask: ${Hexadecimal(payload.wmask.asUInt)}"
    )
  }

  val expected_store = expect_w.head.resp
  when(expected_store.valid) {
    for (j <- 0 until 8) {
      when(expected_store.bits.payload.wmask(j)) {
        val data_match = expected_store.bits.write.wmask(j) &&
          expected_store.bits.payload.wdata(j) === expected_store.bits.write.wdata(j)
        assert(data_match, p"Still expect store byte $j at 0x${Hexadecimal(expected_store.bits.address)}")
      }
    }
  }
}

class StoreUnit(implicit p: SVMParams) extends LSU with HasMemWritePort {
  override def nMemWritePorts: Int = 1

  io.out := io.in

  val store = mem_w.head
  store.valid := lsu_valid(io.in) && in.pipe.lsu.bits.opcode(3)
  val store_address = in.pipe.lsu.bits.phy_addr
  val store_data = do_align_wdata(do_extend(in.pipe.lsu.bits.store_data, in.pipe.lsu.bits.opcode), store_address)
  store.bits.address := store_address
  store.bits.wdata := store_data.asTypeOf(store.bits.wdata)
  store.bits.wmask := store_mask(store_address, in.pipe.lsu.bits.opcode).asBools
  when(store.valid) {
    Log.ref(
      p"[${io.in.bits.id}] mem_store addr: 0x${Hexadecimal(store_address)}, " +
        p"data: 0x${Hexadecimal(store.bits.wdata.asUInt)}, len: ${Hexadecimal(store.bits.wmask.asUInt)}"
    )
  }
}

class AGU(implicit p: SVMParams) extends LSU {
  io.out := io.in

  val lsu = out.pipe.lsu
  lsu.valid := false.B

  // all supported instructions
  val instrs = p.all_insn.filterNot(RVInstructions.system.contains)
  val spikeOps = instrs.map(Instruction.fromSpike).map(_.flatten)

  // collect the load/store/amo instructions based on the Instruction
  val (ops, valid) = instrs
    .zip(spikeOps)
    .collect { case (instr, op) =>
      val result = op.flatMap {
        case Load(address, opcode)        => Some((address, opcode, None))
        case Store(address, opcode, data) => Some((address, opcode, Some(data)))
        case AMO(address, opcode, data)   => Some((address, opcode, Some(data)))
        case _                            => None
      }
      require(result.length <= 1, s"does not support more than one request for $instr")
      result.headOption.map((_, Decoder(in.pipe.instr, instr)))
    }
    .flatten
    .unzip
}

class LAGU(implicit p: SVMParams) extends AGU {
  lsu.bits := DontCare

  when(in_valid && in.should_execute) {
    assert(PopCount(valid) <= 1.U, "multiple load/store/amo?")
    lsu.valid := VecInit(valid).asUInt.orR
    lsu.bits.opcode := Mux1H(valid, ops.map(_._2.U(in)))
    lsu.bits.virt_addr := Mux1H(valid, ops.map(_._1.U(in)))
  }
}

class SAGU(implicit p: SVMParams) extends AGU {
  // set store data
  val (storeOps, storeValid) = ops.zip(valid).filter(_._1._3.isDefined).unzip

  when(in_valid && in.should_execute) {
    lsu.valid := in.pipe.lsu.valid
    lsu.bits.store_data := Mux1H(storeValid, storeOps.map(_._3.get.U(in)))
    // FIXME: SC results. Should check the reservation sets.
    lsu.bits.sc_success := !in.hint.sc_failed
    when(lsu.bits.opcode(5) && in.hint.sc_failed) {
      lsu.valid := false.B
    }
  }
}
