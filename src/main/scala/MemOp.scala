package svm

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import difftest.common.{DifftestMemReadIO, DifftestMemWriteIO}

class MemOpReqAddress extends Bundle {
  val address = UInt(64.W)
}

trait HasMemOpWriteData extends Bundle {
  val wdata = Vec(8, UInt(8.W))
  val wmask = Vec(8, Bool())

  def bytes: Vec[UInt] = wdata.asTypeOf(Vec(8, UInt(8.W)))
}

class MemOpReadPortIO extends Bundle {
  val req = Input(Valid(new MemOpReqAddress))
  val data = Output(Vec(8, UInt(8.W)))
  val miss = Output(Bool())

  // alias (shortcuts)
  def valid: Bool = req.valid
  def address: UInt = req.bits.address
}

class MemOpWriteData extends Bundle with HasMemOpWriteData
class MemOpWritePort extends MemOpReqAddress with HasMemOpWriteData

class MemOpExpectPortIO[T <: Data](gen: T) extends Bundle {
  val req = ValidIO(new Bundle {
    val address = UInt(64.W)
    val payload: T = gen.cloneType
  })
  val resp = Flipped(ValidIO(new Bundle {
    val address = UInt(64.W)
    val payload: T = gen.cloneType
    val write = new MemOpWriteData
  }))

  def payloadType: T = gen.cloneType
}

trait HasMemReadPort { this: Module =>
  def nMemReadPorts: Int

  val mem_r = IO(Vec(nMemReadPorts, Flipped(new MemOpReadPortIO)))
}

trait HasMemWritePort { this: Module =>
  def nMemWritePorts: Int

  val mem_w = IO(Vec(nMemWritePorts, ValidIO(new MemOpWritePort)))
}

trait HasMemWriteExpectation[T <: Data] { this: PipelineStage =>
  def memWriteExpectation: (Int, T)

  val expect_w = IO(Vec(memWriteExpectation._1, new MemOpExpectPortIO(memWriteExpectation._2)))
}

class MemOp(nRead: Int, nWrite: Int, memWriteExpectation: Seq[(Int, Data)])(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val read = Vec(nRead, new MemOpReadPortIO)
    val write = Vec(nWrite, Flipped(ValidIO(new MemOpWritePort)))
    val memory = new Bundle {
      val read = Vec(nRead, Flipped(new CacheReadIO))
      val write = Vec(nWrite, Flipped(new DifftestMemWriteIO(1)))
    }
    val peripheral = Flipped(new RefBusIO)
    val expect = MixedVec(memWriteExpectation.map(e => Flipped(new MemOpExpectPortIO(e._2))))
  })

  io.peripheral.read := false.B
  io.peripheral.write := false.B
  io.peripheral.addr := DontCare
  io.peripheral.wdata := DontCare

  // only use on-chip SRAM in simulation. On FPGAs we are going to use external memories.
  val sram = Option.when(p.isSim || p.isPalladium) {
    val sram = SyncReadMem(p.withSRAM._2 / 8, Vec(8, UInt(8.W)))
    loadMemoryFromFile(sram, sys.env("NOOP_HOME") + "/bootrom/bootrom.txt")
    sram
  }

  // read: flash or memory
  io.read.zipWithIndex.foreach { case (read, i) =>
    val mem_ren = read.valid && p.isInMem(read.address)
    io.memory.read(i).valid := read.valid && p.isInMem(read.address)
    io.memory.read(i).index := read.address(63, 3)
    val mem_rdata = io.memory.read(i).data.asTypeOf(read.data)

    val flash_read = Option.when(p.hasFlash && !p.isFPGA) {
      val flash = difftest.common.DifftestFlash()
      val flash_ren = read.valid && p.isInFlash(read.address)
      val flash_raddr = read.address - p.withFlash.get._1.U
      val flash_rdata = flash.read(flash_ren, flash_raddr).asTypeOf(read.data)
      (flash_ren, flash_rdata)
    }

    val sram_ren = sram.map(_ => read.valid && p.isInSRAM(read.address))
    val sram_raddr = (read.address - p.withSRAM._1.U)(log2Ceil(p.withSRAM._2), 3)
    val sram_rdata = sram.map(_.read(sram_raddr, sram_ren.get).asTypeOf(read.data))

    val peri_ren = read.valid && p.isInPeripheral(read.address)
    when(peri_ren) {
      io.peripheral.read := true.B
      io.peripheral.addr := (read.address - p.peripheralBase.U)(14, 3)
    }
    val peri_rdata = io.peripheral.rdata.asTypeOf(read.data)

    val ren = Seq(mem_ren, peri_ren) ++ flash_read.map(_._1) ++ sram_ren
    assert(!read.valid || VecInit(ren).asUInt.orR, p"out of range memory read ${Hexadecimal(read.address)}")

    val ren_reg = ren.map(en => RegNext(en, false.B))
    val rdata = Seq(mem_rdata, peri_rdata) ++ flash_read.map(_._2) ++ sram_rdata
    read.data := Mux1H(ren_reg, rdata)
    read.miss := ren_reg.head && io.memory.read(i).miss
  }

  // write may be delayed to avoid hazards
  val write_buffers = io.write.zipWithIndex.map { case (write, i) =>
    val buffer = (write.valid, write.bits) +: Seq.fill(p.storeDelay(i))((RegInit(false.B), Reg(new MemOpWritePort)))
    buffer.tail.zip(buffer.dropRight(1)).foreach { case ((v1, w1), (v0, w0)) =>
      v1 := v0
      when(v0) {
        w1 := w0
      }
    }
    buffer
  }

  // write: memory
  val write = write_buffers.map(_.last)
  write.zipWithIndex.foreach { case ((v, w), i) =>
    // At any time, multiple write requests to the memory should not have the same address.
    // To avoid the address conflicts, we invalidate earlier write and bypass data from them.
    val address_match = write.map(ww => ww._1 && ww._2.address(63, 3) === w.address(63, 3))
    // 1) enable: may be invalidated by write requests from later stages (write.drop(i + 1)) with higher priorities
    val is_invalidated = if (write.length == i + 1) false.B else VecInit(address_match.drop(i + 1)).asUInt.orR
    val write_enable = v && !is_invalidated
    // 2) data/mask: collected from earlier stages (write.take(i)) with lower priorities
    val write_mask = WireInit(w.wmask)
    val write_data = WireInit(w.wdata)
    for ((earlier_write, earlier_match) <- write.take(i).map(_._2).zip(address_match.take(i))) {
      for (j <- 0 until 8) {
        when(earlier_match && earlier_write.wmask(j)) {
          write_mask(j) := true.B
          when(!w.wmask(j)) {
            write_data(j) := earlier_write.wdata(j)
          }
        }
      }
    }

    io.memory.write(i) := DontCare
    io.memory.write(i).valid := false.B
    when(write_enable) {
      val mem_wen = p.isInMem(w.address)
      io.memory.write(i).valid := mem_wen
      io.memory.write(i).index := w.address(63, 3)
      io.memory.write(i).data := write_data.asTypeOf(io.memory.write(i).data)
      io.memory.write(i).mask := VecInit(write_mask.map(m => Fill(8, m))).asTypeOf(io.memory.write(i).mask)

      val sram_wen = p.isInSRAM(w.address)
      when(sram_wen) {
        sram.map(_.write((w.address - p.withSRAM._1.U)(log2Ceil(p.withSRAM._2), 3), write_data, write_mask))
      }

      val peri_wen = p.isInPeripheral(w.address)
      when(peri_wen) {
        io.peripheral.write := true.B
        io.peripheral.addr := (w.address - p.peripheralBase.U)(14, 3)
        io.peripheral.wdata := write_data.asUInt
      }
      assert(mem_wen || sram_wen || peri_wen, p"out of range memory write ${Hexadecimal(w.address)}")
    }
  }

  // bypass write to all read ports
  val write_reg = write.map { case (v, w) => RegEnable(w, v) }
  io.read.foreach(read => {
    val do_bypass = write.map { case (v, w) => read.valid && v && read.address(63, 3) === w.address(63, 3) }
    val do_bypass_reg = RegNext(VecInit(do_bypass), VecInit.fill(write.length)(false.B))
    // write ports do have priorities
    for ((bypass, w) <- do_bypass_reg.zip(write_reg)) {
      for ((rdata, (wdata, mask)) <- read.data.zip(w.wdata.zip(w.wmask))) {
        when(bypass && mask) {
          rdata := wdata
        }
      }
    }
  })

  // write may be expected by a speculated load
  io.expect.zipWithIndex.foreach { case (expect, i) =>
    val (delay, _) = memWriteExpectation(i)
    val buffer =
      (expect.req.valid, expect.req.bits) +: Seq.fill(delay)((RegInit(false.B), Reg(chiselTypeOf(expect.req.bits))))
    for ((next, prev) <- buffer.tail.zip(buffer.dropRight(1))) {
      next._1 := prev._1
      when(prev._1) {
        next._2 := prev._2
      }
    }
    // We collect the corresponding write values here. Since the latest store takes effect,
    // we must wait for the completion of all prior stores to confirm the correctness of the expectation.
    val collected = Seq.fill(delay)(Reg(new MemOpWriteData))
    for ((((v0, w0), c), j) <- buffer.dropRight(1).zip(collected).zipWithIndex) {
      val group_size = io.expect.length / p.nCommits
      // For the last store matching stage, filter only prior stores.
      // For instructions in the group, a load should expect only stores from prior instructions.
      // Otherwise, it expects all stores because of the stores are earlier than this load.
      val match_source = if (j == buffer.length - 2) write.take(i / group_size) else write
      val match_write = match_source.map(w => is_match(w0.address)(w._1, w._2))
      for (k <- 0 until 8) {
        val match_vec = match_write.map(_(k))
        val is_match = v0 && match_vec.fold(false.B)(_ || _)
        c.wmask(k) := is_match
        // For j > 0, the write mask/data is naturally propagated (with lower priorities)
        if (j > 0) {
          when(collected(j - 1).wmask(k)) {
            c.wmask(k) := true.B
            c.wdata(k) := collected(j - 1).wdata(k)
          }
        }
        when(is_match) {
          // Stores at later stages have higher priorities
          for ((m, s) <- match_vec.zip(match_source.map(_._2.wdata(k)))) {
            when(m) {
              c.wdata(k) := s
            }
          }
        }
      }
    }
    expect.resp.valid := buffer.last._1
    expect.resp.bits.address := buffer.last._2.address
    expect.resp.bits.payload := buffer.last._2.payload
    expect.resp.bits.write := collected.last
  }

  private def is_match(address: UInt)(w: (Bool, MemOpWritePort)): UInt = {
    Mux(w._1 && w._2.address(63, 3) === address(63, 3), w._2.wmask.asUInt, 0.U(8.W))
  }
}
