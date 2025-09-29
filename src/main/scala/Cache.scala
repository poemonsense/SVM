package svm

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public, Definition, Instance}
import chisel3.util._
import difftest.common.{DifftestMem, DifftestMemReadIO, DifftestMemWriteIO}

case class CacheParams(
  cacheSize: Int,
  nWays: Int,
  nRead: Int,
  nWrite: Int,
  nWords: Int = 1, // in words
  refillOnReadMiss: Boolean = false,
  refillOnWriteMiss: Boolean = false,
  physicalAddressSpace: Int = 32,
  useSimpleSRAMs: Boolean = false,
  numSRAMBanks: Int = 1,
  numSRAMReadPorts: Int = 0,
  replacement: String = "none",
)(implicit p: SVMParams) {
  require(cacheSize % (blockSize * nWays) == 0, s"cacheSize must be divisible by ($blockSize * $nWays)")
  require(isPow2(numSRAMBanks), s"numSRAMBanks ($numSRAMBanks) must be power of 2")

  def memBase: Long = p.memBase

  def wordSize: Int = 8
  def blockSize: Int = nWords * wordSize
  def nSets: Int = cacheSize / blockSize / nWays

  def wayIndexWidth = log2Ceil(nWays)

  def wordOffsetWidth: Int = log2Ceil(nWords)
  def setIndexWidth: Int = log2Ceil(nSets)
  def tagWidth: Int = physicalAddressSpace - log2Ceil(wordSize) - setIndexWidth - wordOffsetWidth

  // index: word-aligned
  def indexToOffset(index: UInt): UInt = if (wordOffsetWidth > 0) index(wordOffsetWidth - 1, 0) else 0.U
  def indexToAddr(index: UInt): UInt = index(index.getWidth - 1, wordOffsetWidth)
  def indexToSet(index: UInt): UInt = addrToSet(indexToAddr(index))
  def indexToTag(index: UInt): UInt = addrToTag(indexToAddr(index))

  // addr: beat-aligned
  def addrW: Int = physicalAddressSpace - log2Ceil(wordSize) - wordOffsetWidth
  def addrToSet(addr: UInt): UInt = addr(setIndexWidth - 1, 0)
  def addrToTag(addr: UInt): UInt = addr(Seq(addr.getWidth, addrW).min - 1, setIndexWidth)

  // read ports
  def maxSRAMWayMask: Int = 8

  def log(op: String, message: Printable): Unit = Log.ref(p"cache_$op " + message)(p)
  def log(op: String, set: UInt, tag: UInt, message: Printable): Unit = {
    log(op, p"[${Hexadecimal(set)}](${Hexadecimal(tag)}) " + message)
  }
  def warn(message: Printable): Unit = printf(p"warning: $message\n")
  def warn(cond: Bool, message: Printable): Unit = when(cond) { warn(message) }

  lazy val replacer = ReplacementPolicy.fromString(replacement, nWays)
  def replace_way(cond: UInt, state: UInt): UInt = {
    val way = replacer.map(_.get_replace_way(state, cond)).getOrElse(PriorityEncoder(cond))
    require(way.getWidth == log2Ceil(nWays), s"${way.getWidth} != ${log2Ceil(nWays)}")
    way
  }
  def replace_way(sets: Seq[CacheMetadata], state: UInt): (Bool, UInt) = {
    val invalid = VecInit(sets.map(s => !s.valid))
    val clean = VecInit(sets.map(s => !s.dirty)).asUInt
    val dirtyEvicted = VecInit(sets.map(_.dirtyEvicted)).asUInt // must be clean now && (previously evicted && dirty)
    val evicted = VecInit(sets.map(_.evicted)).asUInt // must be clean
    val cond = invalid.asUInt.orR || clean.orR
    val sel = Mux(
      invalid.asUInt.orR,
      PriorityEncoder(invalid),
      Mux(
        dirtyEvicted.orR,
        replace_way(dirtyEvicted, state),
        Mux(evicted.orR, replace_way(evicted, state), replace_way(clean, state)),
      ),
    )
    (cond, sel)
  }
}

class CacheRefillEvict()(implicit p: CacheParams) extends Bundle {
  val addr = UInt(p.addrW.W)
  val wen = Bool()
  val data = Vec(p.nWords, UInt(64.W))
}

class CacheMetadata(implicit p: CacheParams) extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val evicted = Bool()
  val dirtyEvicted = Bool()
  val tag = UInt(p.tagWidth.W)

  def hit(target: UInt): Bool = valid && target === tag

  override def toPrintable: Printable = p"V($valid) D($dirty) E($evicted$dirtyEvicted) TAG(${Hexadecimal(tag)})"
}

class CacheData extends Bundle {
  val v = Bool()
  val d = UInt(8.W)
}

class CacheSRAMReadIO[T <: Data](gen: T)(implicit p: CacheParams) extends Bundle {
  val enable = Output(Bool())
  val set = Output(UInt(p.setIndexWidth.W))
  val data = Input(UInt(gen.getWidth.W))
  val miss = Input(Bool())

  def elementWidth = gen match {
    case value: Vec[_] => gen.getWidth / value.length
    case _             => gen.getWidth
  }
  def apply(_set: UInt, _enable: Bool): T = {
    set := _set
    enable := _enable
    data.asTypeOf(gen)
  }
}

class CacheSRAMWriteIO[T <: Data](gen: T)(implicit p: CacheParams) extends Bundle {
  val enable = Output(Bool())
  val set = Output(UInt(p.setIndexWidth.W))
  val data = Output(UInt(gen.getWidth.W))
  val mask = Option.when(gen.isInstanceOf[Vec[_]])(Output(UInt(gen.asInstanceOf[Vec[_]].length.W)))

  def apply(_set: UInt, _data: T): Unit = {
    enable := true.B
    set := _set
    data := _data.asUInt
  }

  def apply(_set: UInt, _data: T, _mask: Seq[Bool]): Unit = {
    apply(_set, _data)
    require(mask.isDefined)
    mask.get := VecInit(_mask).asUInt
  }
}

class CacheMetaSRAMReadIO(implicit p: CacheParams) extends CacheSRAMReadIO(Vec(p.nWays, new CacheMetadata))
class CacheMetaSRAMWriteIO(implicit p: CacheParams) extends CacheSRAMWriteIO(Vec(p.nWays, new CacheMetadata))
class CacheDataSRAMReadIO(implicit p: CacheParams) extends CacheSRAMReadIO(Vec(p.blockSize, new CacheData))
class CacheDataSRAMWriteIO(implicit p: CacheParams) extends CacheSRAMWriteIO(Vec(p.blockSize, new CacheData))

class CacheReadIO extends DifftestMemReadIO(1) {
  val miss = Output(Bool())
}

class CacheWriteIO extends DifftestMemWriteIO(1)

class Cache(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val read = Vec(p.nRead, new CacheReadIO)
    val write = Vec(p.nWrite, new CacheWriteIO)
    val dut = Input(Valid(new CacheRefillEvict))
    val reset_done = Output(Bool())
  })

  // Read
  val load_pipe = Seq.fill(p.nRead)(Module(new LoadPipe))
  for ((pipe, i) <- load_pipe.zipWithIndex) {
    pipe.io.read <> io.read(i)
    pipe.io.write := io.write
  }
  PerfCounter(PopCount(load_pipe.map(_.io.perf.sram_miss)), "cache_read_sram_miss")
  PerfCounter(PopCount(load_pipe.map(_.io.perf.cache_miss)), "cache_read_cache_miss")

  // Write
  val store_pipe = Seq.fill(p.nWrite)(Module(new StorePipe))
  for ((pipe, i) <- store_pipe.zipWithIndex) {
    pipe.io.write := io.write(i)
  }

  // Refill and Evict
  val dut_pipe = Module(new RefillEvictPipe)
  dut_pipe.io.dut := io.dut
  dut_pipe.io.write := io.write

  // replacement
  val replacer = Module(new ReplacerState(p.nRead + p.nWrite + 1))

  val repl_set = io.read.map(r => (p.indexToSet(r.index), r.valid)) ++
    io.write.map(w => (p.indexToSet(w.index), w.valid)) :+
    (p.addrToSet(io.dut.bits.addr), io.dut.valid)
  val repl_pipe = load_pipe.map(_.io.repl) ++ store_pipe.map(_.io.repl) :+ dut_pipe.io.repl
  for ((((set, valid), pipe), port) <- repl_set.zip(repl_pipe).zip(replacer.io.ports)) {
    port.read.valid := valid
    port.read.bits := set
    pipe.state := port.state
    port.access := pipe.touch
  }

  io.reset_done := replacer.io.reset_done

  // SRAMs
  val meta_read = (store_pipe.map(_.io.meta.read) :+ dut_pipe.io.meta.read) ++ load_pipe.map(_.io.meta.read)
  val meta_write = load_pipe.flatMap(_.io.meta.write) ++ store_pipe.map(_.io.meta.write) :+ dut_pipe.io.meta.write
  val data_read = dut_pipe.io.data.read +: load_pipe.map(_.io.data.read)
  val data_write = load_pipe.flatMap(_.io.data.write) ++ store_pipe.map(_.io.data.write) :+ dut_pipe.io.data.write
  CacheSRAM.optimize(meta_read, meta_write, data_read, data_write)
}

class ReplacerNextState(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt((p.nWays - 1).W))
    val access = Input(UInt(p.wayIndexWidth.W))
    val out = Output(UInt((p.nWays - 1).W))
  })

  io.out := p.replacer.get.get_next_state(io.in, io.access)
}

class ReplacerRegfile(size: Int, dataWidth: Int, numPorts: Int) extends Module {
  val io = IO(
    Vec(
      numPorts,
      new Bundle {
        val read = Input(Valid(UInt(log2Ceil(size).W)))
        val data = Output(UInt(dataWidth.W))
        val write = Input(Valid(UInt(dataWidth.W)))
      },
    )
  )

  val data = RegInit(VecInit.fill(size)(0.U(dataWidth.W)))

  val s1_write_reg = io.map(port => {
    port.data := RegEnable(data(port.read.bits), port.read.valid)

    val s0_write_en_reg = RegNext(port.read.valid, false.B)
    val s0_write_addr_reg = RegEnable(port.read.bits, port.read.valid)

    val s1_write_en_reg = RegNext(s0_write_en_reg && port.write.valid)
    val s1_write_addr_reg = RegEnable(s0_write_addr_reg, s0_write_en_reg && port.write.valid)
    val s1_write_data_reg = RegEnable(port.write.bits, s0_write_en_reg && port.write.valid)

    (s1_write_en_reg, s1_write_addr_reg, s1_write_data_reg)
  })

  val s1_write_addr_one_hot = s1_write_reg.map(w => WireInit(UIntToOH(w._2)))
  s1_write_addr_one_hot.foreach(dontTouch(_))

  for (i <- 0 until size) {
    val s1_write_en_vec = VecInit(s1_write_reg.zip(s1_write_addr_one_hot).map { case (w, a) => w._1 && a(i) })
    val s1_write_en = WireInit(s1_write_en_vec.asUInt)
    when(s1_write_en.orR) {
      data(i) := Mux1H(s1_write_en, s1_write_reg.map(_._3))
    }
  }

  var read_index = 0
  def read(addr: UInt, cond: Bool): UInt = {
    io(read_index).read.valid := cond
    io(read_index).read.bits := addr
    read_index += 1
    io(read_index - 1).data
  }

  var write_index = 0
  def write(data: UInt, cond: Bool): Unit = {
    io(write_index).write.valid := cond
    io(write_index).write.bits := data
    write_index += 1
  }
}

object Replacer {
  def nextState(in: UInt, access: UInt)(implicit p: CacheParams): UInt = {
    val next_state = Module(new ReplacerNextState)
    next_state.io.in := in
    next_state.io.access := access
    next_state.io.out
  }

  def regfile(size: Int, dataWidth: Int, numPorts: Int): ReplacerRegfile = {
    Module(new ReplacerRegfile(size, dataWidth, numPorts))
  }
}

class ReplacerState(numPorts: Int)(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val ports = Vec(
      numPorts,
      new Bundle {
        val read = Input(Valid(UInt(p.setIndexWidth.W)))
        val state = Output(UInt((p.nWays - 1).W))
        val access = Input(Valid(UInt(p.wayIndexWidth.W)))
      },
    )
    val reset_done = Output(Bool())
  })

  io.reset_done := true.B
  val reset_counter = RegInit(p.nSets.U)
  val reset_counter_next = reset_counter - 1.U
  when(reset_counter =/= 0.U) {
    io.reset_done := false.B
    reset_counter := reset_counter_next
  }

  if (p.replacer.isDefined) {
    val maxRegfileSize = 64
    val memSize = if (p.nSets > maxRegfileSize) maxRegfileSize else p.nSets
    val numBanks = if (p.nSets > maxRegfileSize) p.nSets / maxRegfileSize else 1
    val state = Seq.fill(numBanks)(Replacer.regfile(memSize, p.nWays - 1, numPorts))
    def toBankIndex(addr: UInt): UInt = if (numBanks > 1) addr(log2Ceil(numBanks) - 1, 0) else 0.U(1.W)
    def toBankOffset(addr: UInt): UInt = (addr >> log2Ceil(numBanks)).asUInt

    for (port <- io.ports) {
      // stage 0: read req
      val bank_index_oh = WireInit(UIntToOH(toBankIndex(port.read.bits)))
      val bank_offset = WireInit(toBankOffset(port.read.bits))

      // partial read data is pipelined between stage 0 and stage 1
      val state_read_reg = state.zipWithIndex.map { case (s, i) =>
        s.read(bank_offset, port.read.valid && bank_index_oh(i))
      }

      // stage 1: read resp && write
      val bank_index_oh_reg = RegEnable(bank_index_oh, port.read.valid)
      port.state := Mux1H(bank_index_oh_reg, state_read_reg)

      val next_state = Replacer.nextState(port.state, port.access.bits)
      state.foreach(_.write(next_state, port.access.valid))
    }
  } else {
    io.ports.foreach(_.state := 0.U)
  }
}

class LoadPipe(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val read = new CacheReadIO
    val write = Vec(p.nWrite, new DifftestMemWriteIO(1))
    val meta = new Bundle {
      val read = new CacheMetaSRAMReadIO
      val write = Option.when(p.refillOnReadMiss)(new CacheMetaSRAMWriteIO)
    }
    val data = new Bundle {
      val read = Vec(p.nWays, new CacheDataSRAMReadIO)
      val write = Option.when(p.refillOnReadMiss)(Vec(p.nWays, new CacheDataSRAMWriteIO))
    }
    val repl = new Bundle {
      val state = Input(UInt((p.nWays - 1).W))
      val touch = ValidIO(UInt(p.wayIndexWidth.W))
    }
    val perf = Output(new Bundle {
      val sram_miss = Bool()
      val cache_miss = Bool()
    })
  })

  io.meta.write.foreach(_ := DontCare)
  io.meta.write.foreach(_.enable := false.B)
  io.data.write.foreach(_.foreach(_ := DontCare))
  io.data.write.foreach(_.foreach(_.enable := false.B))

  // for bypass
  val s1_write_reg = RegNext(io.write.asTypeOf(Output(chiselTypeOf(io.write))))

  val s0_set = p.indexToSet(io.read.index)
  val s0_bypass = s1_write_reg.map(w => w.valid && w.index === io.read.index)
  assert(PopCount(s0_bypass) <= 1.U, "cannot handle multiple bypass now")
  val s0_bypass_mask = s0_bypass.zip(s1_write_reg).map { case (by, w) => Mux(by, w.mask.asUInt, 0.U) }.reduce(_ | _)
  val s0_bypass_data = s0_bypass.zip(s1_write_reg).map { case (by, w) => Mux(by, w.data.asUInt, 0.U) }.reduce(_ | _)

  val s1_valid = RegNext(io.read.valid, false.B)
  val s1_bits = RegEnable(io.read.asTypeOf(Output(chiselTypeOf(io.read))), io.read.valid)
  val s1_meta = io.meta.read(s0_set, io.read.valid)
  val s1_data = io.data.read.map(_(s0_set, io.read.valid))
  val s1_set = p.indexToSet(s1_bits.index)
  val s1_tag = p.indexToTag(s1_bits.index)
  val s1_offset = p.indexToOffset(s1_bits.index)

  val s1_full_bypass = RegEnable(s0_bypass_mask.andR, io.read.valid)
  val s1_bypass_mask = RegEnable(s0_bypass_mask, io.read.valid)
  val s1_bypass_data = RegEnable(s0_bypass_data, io.read.valid)

  val s1_hits = VecInit(s1_meta.map(_.hit(s1_tag)))
  val s1_hit_data_full = Mux1H(s1_hits, s1_data)
  val s1_hit_data_valid = VecInit(s1_hit_data_full.map(_.v)).asTypeOf(Vec(p.nWords, UInt(8.W)))(s1_offset).andR
  val s1_hit_data = VecInit(s1_hit_data_full.map(_.d)).asTypeOf(Vec(p.nWords, UInt(64.W)))(s1_offset)
  val s1_read_data = (s1_bypass_mask & s1_bypass_data) | ((~s1_bypass_mask) & s1_hit_data)

  val s1_meta_sram_miss = io.meta.read.miss
  val s1_data_sram_miss = Mux1H(s1_hits, io.data.read.map(_.miss))
  val s1_sram_miss = s1_meta_sram_miss || s1_data_sram_miss
  io.perf.sram_miss := s1_valid && s1_sram_miss

  // cache hit
  val s1_hit = s1_full_bypass || s1_hits.asUInt.orR && s1_hit_data_valid
  when(s1_valid && s1_hit) {
    when(s1_meta_sram_miss) {
      p.log("read", s1_set, s1_tag, p"meta sram miss")
    }.elsewhen(s1_data_sram_miss) {
      p.log("read", s1_set, s1_tag, p"data sram miss")
    }.otherwise {
      p.log("read", s1_set, s1_tag, p"=> ${Hexadecimal(s1_read_data)}")
      assert(PopCount(s1_hits) <= 1.U, p"read multiple hit at cache[${Hexadecimal(s1_set)}](${Hexadecimal(s1_tag)})")
    }
  }

  io.read.data := s1_read_data.asTypeOf(io.read.data)
  io.read.miss := s1_valid && (!s1_hit || s1_sram_miss)

  io.repl.touch.valid := s1_valid && s1_hits.asUInt.orR && !io.meta.read.miss
  io.repl.touch.bits := OHToUInt(s1_hits)

  // cache miss
  val s1_miss = s1_valid && !s1_sram_miss && !s1_hit
  io.perf.cache_miss := s1_miss

  if (p.refillOnReadMiss) {
    val s1_mem_read_data = if (p.useSimpleSRAMs) {
      // we are not creating the actual data here
      VecInit.fill(p.nWords)(0.U(64.W))
    } else {
      val s0_memory = DifftestMem(BigInt(1) << 31, p.blockSize, 8)
      s0_memory.read(p.indexToAddr(io.read.index - (p.memBase >> 3).U), io.read.valid)
    }

    val s1_miss_data = s1_mem_read_data(s1_offset)
    val (s1_repl_valid, s1_repl_index) = p.replace_way(s1_meta, io.repl.state)
    val s1_repl_index_one_hot = UIntToOH(s1_repl_index)

    val s1_meta_write = Wire(new CacheMetadata)
    s1_meta_write.valid := true.B
    s1_meta_write.dirty := false.B
    s1_meta_write.evicted := false.B
    s1_meta_write.dirtyEvicted := false.B
    s1_meta_write.tag := s1_tag

    val s1_mem_read_bytes = s1_mem_read_data.asTypeOf(Vec(p.blockSize, UInt(8.W)))
    val s1_data_write = Wire(Vec(p.blockSize, new CacheData))
    for (i <- 0 until p.blockSize) {
      s1_data_write(i).v := true.B
      s1_data_write(i).d := s1_mem_read_bytes(i)
    }

    when(s1_miss) {
      assert(s1_repl_valid || s1_hits.asUInt.orR, "cannot find a replacement for read miss")
      val s1_miss_repl_way = Mux(s1_hits.asUInt.orR, s1_hits.asUInt, s1_repl_index_one_hot)

      p.log("read", s1_set, s1_tag, p"miss => ${Hexadecimal(s1_miss_data.asUInt)}")
      io.read.data := s1_miss_data.asTypeOf(io.read.data)

      when(!s1_hits.asUInt.orR) {
        p.log("allocate", s1_set, s1_tag, p"at way-$s1_repl_index: $s1_meta")
        io.meta.write.get.apply(s1_set, VecInit.fill(p.nWays)(s1_meta_write), s1_repl_index_one_hot.asBools)
      }

      for ((w, i) <- io.data.write.get.zipWithIndex) {
        when(s1_miss_repl_way(i)) {
          w(s1_set, s1_data_write, s1_hit_data_full.map(d => !d.v))
        }
      }

      io.repl.touch.valid := true.B
      io.repl.touch.bits := s1_repl_index
    }
  } else {
    when(s1_valid && !s1_hit) {
      p.log("read", s1_set, s1_tag, p"miss")
    }
  }
}

class StorePipe(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val write = new CacheWriteIO
    val meta = new Bundle {
      val read = new CacheMetaSRAMReadIO
      val write = new CacheMetaSRAMWriteIO
    }
    val data = new Bundle {
      val write = Vec(p.nWays, new CacheDataSRAMWriteIO)
    }
    val repl = new Bundle {
      val state = Input(UInt((p.nWays - 1).W))
      val touch = ValidIO(UInt(p.wayIndexWidth.W))
    }
  })

  io.meta.write := DontCare
  io.meta.write.enable := false.B
  io.data.write := DontCare
  io.data.write.foreach(_.enable := false.B)

  val s0_set = p.indexToSet(io.write.index)

  val s1_valid = RegNext(io.write.valid, false.B)
  val s1_bits = RegEnable(io.write.asTypeOf(Output(chiselTypeOf(io.write))), io.write.valid)
  val s1_set = p.indexToSet(s1_bits.index)
  val s1_tag = p.indexToTag(s1_bits.index)
  val s1_meta = io.meta.read(s0_set, io.write.valid)
  val s1_mask = VecInit.tabulate(8)(i => s1_bits.mask.asUInt(8 * i + 7, 8 * i).orR)

  val s1_hits = VecInit(s1_meta.map(_.hit(s1_tag)))
  val s1_hit = s1_hits.asUInt.orR
  val s1_hit_meta = Mux1H(s1_hits, s1_meta)

  when(s1_valid && s1_hit) {
    assert(PopCount(s1_hits) <= 1.U, p"write multiple hit at cache[${Hexadecimal(s1_set)}](${Hexadecimal(s1_tag)})")

    val s1_meta_write = WireInit(s1_meta)
    s1_meta_write.foreach(_.dirty := true.B)
    s1_meta_write.foreach(_.dirtyEvicted := false.B)
    s1_meta_write.foreach(_.evicted := false.B)
    when(s1_valid && !s1_hit_meta.dirty) {
      io.meta.write(s1_set, s1_meta_write, s1_hits)
    }
  }

  io.repl.touch.valid := s1_valid && s1_hit
  io.repl.touch.bits := OHToUInt(s1_hits)

  // cache miss
  assert(!s1_valid || !io.meta.read.miss, "write should never miss on meta.read")
  val s1_miss = s1_valid && !s1_hit
  val s1_meta_write = Wire(new CacheMetadata)
  s1_meta_write.valid := true.B
  s1_meta_write.dirty := true.B
  s1_meta_write.evicted := false.B
  s1_meta_write.dirtyEvicted := false.B
  s1_meta_write.tag := p.indexToTag(s1_bits.index)
  val (s1_repl_valid, s1_repl_index) = p.replace_way(s1_meta, io.repl.state)
  val s1_repl_index_bitmap = UIntToOH(s1_repl_index).asBools.take(p.nWays)
  when(s1_miss) {
    assert(s1_repl_valid, "cannot find a replacement for write miss")
    p.log("allocate", s1_set, s1_tag, p"at way-$s1_repl_index: $s1_meta")
    io.meta.write(s1_set, VecInit.fill(p.nWays)(s1_meta_write), s1_repl_index_bitmap)

    io.repl.touch.valid := true.B
    io.repl.touch.bits := s1_repl_index
  }
  val s1_write_way = Mux(s1_hit, s1_hits.asUInt, VecInit(s1_repl_index_bitmap).asUInt)

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_miss = RegNext(s1_miss, false.B)
  val s2_write_way = RegEnable(s1_write_way, s1_valid)
  val s2_bits = RegEnable(s1_bits, s1_valid)
  val s2_set = p.indexToSet(s2_bits.index)
  val s2_tag = p.indexToTag(s2_bits.index)
  val s2_offset = p.indexToOffset(s2_bits.index)

  val s2_word_mask = (0 until 8).map(i => s2_bits.mask.asUInt(8 * i + 7, 8 * i).orR)
  val s2_mask = VecInit.tabulate(p.blockSize)(i => (i / 8).U === s2_offset && s2_word_mask(i % 8))
  val s2_data_mask = Mux(s2_miss, VecInit.fill(p.blockSize)(true.B), s2_mask)

  val s2_data_write = Wire(Vec(p.blockSize, new CacheData))
  for (i_word <- 0 until p.nWords) {
    val s2_is_this_word = s2_offset === i_word.U
    for (i_byte <- 0 until p.wordSize) {
      val i = i_word * p.wordSize + i_byte
      val v = s2_is_this_word && s2_word_mask(i_byte)
      s2_data_write(i).v := v
      s2_data_write(i).d := Mux(v, s2_bits.data.asUInt(i_byte * 8 + 7, i_byte * 8), 0.U)
    }
  }

  if (p.refillOnWriteMiss) {
    val s2_memory = Seq.fill(p.nWords)(DifftestMem(BigInt(1) << 31, p.blockSize, 8))
    val s2_read_data = VecInit(s2_memory.zipWithIndex.map { case (m, i) =>
      m.read(s1_bits.index + i.U, s2_offset =/= i.U || s1_miss && !s1_mask.asUInt.andR)
    })
    for (((wd, m), d) <- s2_read_data.asTypeOf(Vec(p.blockSize, UInt(8.W))).zip(s2_mask).zip(s2_data_write)) {
      d.v := true.B
      when(!m) {
        d.d := wd
      }
    }
  }

  when(s2_valid) {
    val write_msg = p"<= 0x${Hexadecimal(s2_data_write.asUInt)} (${Hexadecimal(s2_data_mask.asUInt)})"
    when(s2_miss) {
      p.log("write", s2_set, s2_tag, p"miss $write_msg")
    }.otherwise {
      p.log("write", s2_set, s2_tag, write_msg)
    }
    for ((w, i) <- io.data.write.zipWithIndex) {
      when(s2_write_way(i)) {
        w(s2_set, s2_data_write, s2_data_mask)
      }
    }
  }
}

class RefillEvictPipe(implicit p: CacheParams) extends Module {
  val io = IO(new Bundle {
    val dut = Input(Valid(new CacheRefillEvict))
    val write = Vec(p.nWrite, new CacheWriteIO)
    val meta = new Bundle {
      val read = new CacheMetaSRAMReadIO
      val write = new CacheMetaSRAMWriteIO
    }
    val data = new Bundle {
      val read = Vec(p.nWays, new CacheDataSRAMReadIO)
      val write = Vec(p.nWays, new CacheDataSRAMWriteIO)
    }
    val repl = new Bundle {
      val state = Input(UInt((p.nWays - 1).W))
      val touch = ValidIO(UInt(p.wayIndexWidth.W))
    }
  })

  io.meta.write := DontCare
  io.meta.write.enable := false.B
  io.data.write := DontCare
  io.data.write.foreach(_.enable := false.B)

  val s0_set = p.addrToSet(io.dut.bits.addr)
  // match write to invalidate the meta update
  val s0_write_match_vec = io.write.map(w => w.valid && p.indexToAddr(w.index) === io.dut.bits.addr)
  val s0_write_match = VecInit(s0_write_match_vec).asUInt.orR

  val s1_valid = RegNext(io.dut.valid, false.B)
  val s1_bits = RegEnable(io.dut.bits, io.dut.valid)
  val s1_bits_data = s1_bits.data.asTypeOf(Vec(p.blockSize, UInt(8.W)))
  val s1_set = p.addrToSet(s1_bits.addr)
  val s1_meta = io.meta.read(s0_set, io.dut.valid)
  val s1_data = io.data.read.map(_(s0_set, io.dut.valid))
  val s1_write_match = RegEnable(s0_write_match, io.dut.valid)

  val s1_tag = p.addrToTag(s1_bits.addr)
  val s1_hits = s1_meta.map(_.hit(s1_tag))
  val s1_hit = VecInit(s1_hits).asUInt.orR
  val s1_hit_meta = Mux1H(s1_hits, s1_meta)
  val s1_hit_data_full = Mux1H(s1_hits, s1_data)
  val s1_hit_data_valid = VecInit(s1_hit_data_full.map(_.v))
  val s1_hit_data = VecInit(s1_hit_data_full.map(_.d))
  val s1_data_match_vec = s1_bits_data.zip(s1_hit_data_full).map { case (w, h) => !h.v || w === h.d }
  val s1_data_match = VecInit(s1_data_match_vec).asUInt.andR

  // (evict and hit) or refill
  val s1_data_write_valid = s1_valid && (s1_bits.wen && s1_hit || !s1_bits.wen) && !io.meta.read.miss
  val s1_write_way = WireInit(VecInit(s1_hits))
  val s1_data_write = Wire(Vec(p.blockSize, new CacheData))
  for (i <- 0 until p.blockSize) {
    s1_data_write(i).v := true.B
    s1_data_write(i).d := s1_bits_data(i)
  }
  val s1_data_mask = WireInit(VecInit.fill(p.blockSize)(false.B))
  s1_data_mask.zip(s1_hit_data_valid).foreach { case (m, v) => m := !v }
  for ((w, i) <- io.data.write.zipWithIndex) {
    when(s1_data_write_valid && s1_write_way(i)) {
      w(s1_set, s1_data_write, s1_data_mask)
    }
  }

  // Evict
  when(s1_valid && s1_bits.wen && !io.meta.read.miss) {
    p.log("evict", s1_set, s1_tag, p"=> 0x${Hexadecimal(s1_bits.data.asUInt)}")
    when(s1_hit) {
      when(s1_data_match) {
        when(!s1_write_match) {
          val s1_meta_write = WireInit(s1_meta)
          s1_meta_write.foreach(_.dirtyEvicted := s1_hit_meta.dirty)
          s1_meta_write.foreach(_.evicted := true.B)
          s1_meta_write.foreach(_.dirty := false.B)
          io.meta.write(s1_set, s1_meta_write, s1_hits)
        }
      }.otherwise {
        assert(s1_hit_meta.dirty, "evict data mismatch")
      }
      s1_data_mask.zip(s1_hit_data_valid).foreach { case (m, v) => m := !v }
    }
  }
  PerfCounter(s1_valid && s1_bits.wen, "cache_evict")
  PerfCounter(s1_valid && s1_bits.wen && (!s1_hit || io.meta.read.miss), "cache_evict_miss")
  PerfCounter(s1_valid && s1_bits.wen && !s1_hit, "cache_evict_miss_cache")
  PerfCounter(s1_valid && s1_bits.wen && io.meta.read.miss, "cache_evict_miss_sram")

  // only refill updates the PLRU state
  io.repl.touch.valid := s1_valid && !s1_bits.wen && !io.meta.read.miss
  io.repl.touch.bits := OHToUInt(s1_hits)

  // Refill
  when(s1_valid && !s1_bits.wen && !io.meta.read.miss) {
    p.log("refill", s1_set, s1_tag, p"=> 0x${Hexadecimal(s1_bits_data.asUInt)}")
    when(s1_hit) {
      when(s1_hit_meta.dirty) {
        assert(!s1_hit_meta.evicted && !s1_hit_meta.dirtyEvicted, "dirty block should not be marked as evicted")
      }.otherwise {
        assert(
          s1_data_match,
          p"refill data mismatch 0x${Hexadecimal(VecInit(s1_hits).asUInt)} 0x${Hexadecimal(s1_hit_data.asUInt)}",
        )
        when(s1_hit_meta.evicted || s1_hit_meta.dirtyEvicted) {
          val s1_meta_write = WireInit(s1_meta)
          s1_meta_write.foreach(_.evicted := false.B)
          s1_meta_write.foreach(_.dirtyEvicted := false.B)
          io.meta.write(s1_set, s1_meta_write, s1_hits)
        }
      }
      s1_data_mask.zip(s1_hit_data_valid).foreach { case (m, v) => m := !v }
    }.otherwise {
      val (repl_valid, repl_index) = p.replace_way(s1_meta, io.repl.state)
      io.repl.touch.bits := repl_index
      assert(repl_valid, "cannot find a replacement for refill")
      p.log("allocate", s1_set, s1_tag, p"at way-$repl_index: $s1_meta")
      val repl_meta = Wire(new CacheMetadata)
      repl_meta.valid := true.B
      repl_meta.dirty := false.B
      repl_meta.evicted := false.B
      repl_meta.dirtyEvicted := false.B
      repl_meta.tag := s1_tag
      io.meta.write(s1_set, VecInit.fill(p.nWays)(repl_meta), UIntToOH(repl_index).asBools.take(p.nWays))
      s1_write_way.zipWithIndex.foreach { case (w, i) => w := repl_index === i.U }
      s1_data_mask.foreach(_ := true.B)
    }
  }
  PerfCounter(s1_valid && !s1_bits.wen, "cache_refill")
  PerfCounter(s1_valid && !s1_bits.wen && io.meta.read.miss, "cache_refill_miss_sram")
}

class CacheSRAMTemplate(numRead: Int, numWrite: Int, nSets: Int, nWays: Int, dataWidth: Int)(implicit p: CacheParams)
  extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, Flipped(new CacheSRAMReadIO(Vec(nWays, UInt(dataWidth.W)))))
    val write = Vec(numWrite, Flipped(new CacheSRAMWriteIO(Vec(nWays, UInt(dataWidth.W)))))
  })

  val numSRAMs = if (p.useSimpleSRAMs) numRead else 1

  require(!p.useSimpleSRAMs || nWays < p.maxSRAMWayMask || nWays % p.maxSRAMWayMask == 0)
  val numDataBanks = if (p.useSimpleSRAMs && nWays > p.maxSRAMWayMask) nWays / p.maxSRAMWayMask else 1
  val dataVecWidth = if (p.useSimpleSRAMs && nWays > p.maxSRAMWayMask) p.maxSRAMWayMask else nWays
  val dataType = Vec(dataVecWidth, UInt(dataWidth.W))

  val sramLibs = Seq.fill(numSRAMs)(Seq.fill(numDataBanks)(SyncReadMem(nSets, dataType)))

  // partitions: data banks
  val readPartitions = io.read.zipWithIndex.groupBy(_._2 % sramLibs.length).values.map(_.map(_._1))

  // write ports are delayed for one clock cycle to improve timing
  val sram_write_reg = io.write.map(RegNext(_))

  // read and write to SRAMs
  for ((ports, sram) <- readPartitions.zip(sramLibs)) {
    // read ports are partially connected to each SRAM
    for (r <- ports) {
      val read_data = WireInit(VecInit(sram.flatMap(_.read(r.set, r.enable))))

      // we need to bypass the write to read data
      val read_index_reg = RegNext(r.set)
      val write_match = VecInit(sram_write_reg.map(w => w.enable && w.set === read_index_reg))
      for (i <- 0 until dataVecWidth) {
        val m = VecInit(write_match.zip(sram_write_reg).map(x => x._1 && x._2.mask.get(i)))
        when(m.asUInt.orR) {
          read_data(i) := Mux1H(m, sram_write_reg.map(_.data.asTypeOf(dataType)(i)))
        }
      }

      r.data := read_data.asUInt
      r.miss := false.B
    }

    // every SRAM is connected with all write ports
    for (w <- sram_write_reg) {
      when(w.enable) {
        val data = w.data.asTypeOf(Vec(nWays, UInt(dataWidth.W)))
        val mask = w.mask.get.asBools
        for (i <- 0 until numDataBanks) {
          val lower = i * dataVecWidth
          val upper = lower + dataVecWidth
          sram(i).write(w.set, VecInit(data.slice(lower, upper)), mask.slice(lower, upper))
        }
      }
    }
  }
}

class CacheSRAM(nSets: Int, metaRead: Int, metaWrite: Int, dataRead: Int, dataWrite: Int)(implicit p: CacheParams)
  extends Module {
  val io = IO(new Bundle {
    val meta = new Bundle {
      val read = Vec(metaRead, Flipped(new CacheMetaSRAMReadIO))
      val write = Vec(metaWrite, Flipped(new CacheMetaSRAMWriteIO))
    }
    val data = new Bundle {
      val read = Vec(dataRead, Vec(p.nWays, Flipped(new CacheDataSRAMReadIO)))
      val write = Vec(dataWrite, Vec(p.nWays, Flipped(new CacheDataSRAMWriteIO)))
    }
  })

  // reset: re-use the first port
  val firstMetaWrite = WireInit(io.meta.write.head)
  val reset_mask = Wire(Vec(p.nWays, Bool()))
  reset_mask := Seq.fill(p.nWays)(true.B)
  dontTouch(reset_mask)
  val reset_counter = RegInit(nSets.U)
  val reset_counter_next = reset_counter - 1.U
  when(reset_counter =/= 0.U) {
    reset_counter := reset_counter_next
    firstMetaWrite(reset_counter_next, 0.U.asTypeOf(Vec(p.nWays, new CacheMetadata)), reset_mask)
  }

  private def connect_sram[T <: Data](
    readPorts: Seq[CacheSRAMReadIO[_]],
    writePorts: Seq[CacheSRAMWriteIO[_]],
    nWays: Int,
    dataWidth: Int,
  ): Unit = {
    val sram = Module(new CacheSRAMTemplate(readPorts.length, writePorts.length, nSets, nWays, dataWidth))
    sram.io.read.zip(readPorts).foreach(x => x._1 <> x._2)
    sram.io.write.zip(writePorts).foreach(x => x._1 := x._2)
  }

  connect_sram(io.meta.read, firstMetaWrite +: io.meta.write.tail, p.nWays, (new CacheMetadata).getWidth)
  for (i <- 0 until p.nWays) {
    connect_sram(io.data.read.map(_(i)), io.data.write.map(_(i)), p.blockSize, (new CacheData).getWidth)
  }
}

object CacheSRAM {
  def cached(
    metaRead: Seq[CacheMetaSRAMReadIO],
    metaWrite: Seq[CacheMetaSRAMWriteIO],
    dataRead: Seq[Vec[CacheDataSRAMReadIO]],
    dataWrite: Seq[Vec[CacheDataSRAMWriteIO]],
  )(implicit p: CacheParams): (
    Seq[CacheMetaSRAMReadIO],
    Seq[CacheMetaSRAMWriteIO],
    Seq[Vec[CacheDataSRAMReadIO]],
    Seq[Vec[CacheDataSRAMWriteIO]],
  ) = {
    if (p.numSRAMReadPorts < metaRead.length || p.numSRAMReadPorts < dataRead.length) {
      val cached = Module(new CachedSRAMPorts(metaRead.length, metaWrite.length, dataRead.length, dataWrite.length))
      cached.io.in.meta.read <> metaRead
      cached.io.in.meta.write <> metaWrite
      cached.io.in.data.read <> dataRead
      cached.io.in.data.write <> dataWrite
      (cached.io.out.meta.read, metaWrite, cached.io.out.data.read, dataWrite)
    } else {
      (metaRead, metaWrite, dataRead, dataWrite)
    }
  }

  def banked(
    metaRead: Seq[CacheMetaSRAMReadIO],
    metaWrite: Seq[CacheMetaSRAMWriteIO],
    dataRead: Seq[Vec[CacheDataSRAMReadIO]],
    dataWrite: Seq[Vec[CacheDataSRAMWriteIO]],
  )(implicit p: CacheParams): Seq[
    (
      Seq[CacheMetaSRAMReadIO],
      Seq[CacheMetaSRAMWriteIO],
      Seq[Vec[CacheDataSRAMReadIO]],
      Seq[Vec[CacheDataSRAMWriteIO]],
    )
  ] = {
    if (p.numSRAMBanks > 1) {
      val banked = Module(new BankedSRAMPorts(metaRead.length, metaWrite.length, dataRead.length, dataWrite.length))
      banked.io.in.meta.read <> metaRead
      banked.io.in.meta.write <> metaWrite
      banked.io.in.data.read <> dataRead
      banked.io.in.data.write <> dataWrite
      banked.io.out.map(o => (o.meta.read, o.meta.write, o.data.read, o.data.write))
    } else {
      Seq((metaRead, metaWrite, dataRead, dataWrite))
    }
  }

  private val reduceDefs = scala.collection.mutable.Map.empty[(Int, Int, Int, Int), Definition[ReduceSRAMPorts]]
  def reduce(
    metaRead: Seq[CacheMetaSRAMReadIO],
    metaWrite: Seq[CacheMetaSRAMWriteIO],
    dataRead: Seq[Vec[CacheDataSRAMReadIO]],
    dataWrite: Seq[Vec[CacheDataSRAMWriteIO]],
  )(implicit p: CacheParams): (
    Seq[CacheMetaSRAMReadIO],
    CacheMetaSRAMWriteIO,
    Seq[Vec[CacheDataSRAMReadIO]],
    Vec[CacheDataSRAMWriteIO],
  ) = {
    val key = (metaRead.length, metaWrite.length, dataRead.length, dataWrite.length)
    val reduceDef = reduceDefs.getOrElseUpdate(key, Definition(new ReduceSRAMPorts(key)))
    val reduce = Instance(reduceDef)
    reduce.io.in.meta.read <> metaRead
    reduce.io.in.meta.write <> metaWrite
    reduce.io.in.data.read <> dataRead
    reduce.io.in.data.write <> dataWrite
    assert(!reduce.io.in.meta.conflict, "meta write port conflicts")
    reduce.io.in.data.conflict.asBools.zipWithIndex.foreach { case (conflict, i) =>
      assert(!conflict, s"data_$i write port conflicts")
    }
    (reduce.io.out.meta.read, reduce.io.out.meta.write, reduce.io.out.data.read, reduce.io.out.data.write)
  }

  def base(
    metaRead: Seq[CacheMetaSRAMReadIO],
    metaWrite: Seq[CacheMetaSRAMWriteIO],
    dataRead: Seq[Vec[CacheDataSRAMReadIO]],
    dataWrite: Seq[Vec[CacheDataSRAMWriteIO]],
    size: Int,
  )(implicit p: CacheParams): CacheSRAM = {
    val sram = Module(new CacheSRAM(size, metaRead.length, metaWrite.length, dataRead.length, dataWrite.length))
    sram.io.meta.read <> metaRead
    sram.io.meta.write <> metaWrite
    sram.io.data.read <> dataRead
    sram.io.data.write <> dataWrite
    sram
  }

  def optimize(
    metaRead: Seq[CacheMetaSRAMReadIO],
    metaWrite: Seq[CacheMetaSRAMWriteIO],
    dataRead: Seq[Vec[CacheDataSRAMReadIO]],
    dataWrite: Seq[Vec[CacheDataSRAMWriteIO]],
  )(implicit p: CacheParams): Unit = {
    val (cmR, cmW, cdR, cdW) = cached(metaRead, metaWrite, dataRead, dataWrite)
    val banks = banked(cmR, cmW, cdR, cdW)
    val srams = banks.map { case (mR, mW, dR, dW) =>
      val (mRX, mWX, dRX, dWX) = reduce(mR, mW, dR, dW)
      base(mRX, Seq(mWX), dRX, Seq(dWX), p.nSets / banks.length)
    }
  }
}

class BankedSRAMPorts(metaRead: Int, metaWrite: Int, dataRead: Int, dataWrite: Int)(implicit p: CacheParams)
  extends Module {
  private val size = p.nSets

  val io = IO(new Bundle {
    val in = new Bundle {
      val meta = new Bundle {
        val read = Vec(metaRead, Flipped(new CacheMetaSRAMReadIO))
        val write = Vec(metaWrite, Flipped(new CacheMetaSRAMWriteIO))
      }
      val data = new Bundle {
        val read = Vec(dataRead, Vec(p.nWays, Flipped(new CacheDataSRAMReadIO)))
        val write = Vec(dataWrite, Vec(p.nWays, Flipped(new CacheDataSRAMWriteIO)))
      }
    }
    val out = Vec(
      p.numSRAMBanks,
      new Bundle {
        val meta = new Bundle {
          val read = Vec(metaRead, new CacheMetaSRAMReadIO)
          val write = Vec(metaWrite, new CacheMetaSRAMWriteIO)
        }
        val data = new Bundle {
          val read = Vec(dataRead, Vec(p.nWays, new CacheDataSRAMReadIO))
          val write = Vec(dataWrite, Vec(p.nWays, new CacheDataSRAMWriteIO))
        }
      },
    )
  })

  private val offsetWidth = log2Ceil(p.numSRAMBanks)
  private val indexWidth = log2Ceil(size) - offsetWidth

  private def getOffset(addr: UInt) = if (offsetWidth > 0) addr(offsetWidth - 1, 0) else 0.U
  private def getIndex(addr: UInt) = addr(offsetWidth + indexWidth - 1, offsetWidth)
  private def parseAddr(addr: UInt): (UInt, UInt) = (getIndex(addr), getOffset(addr))

  def read[T <: CacheSRAMReadIO[_]](in: T, out: Seq[T]): Unit = {
    val (index, offset) = parseAddr(in.set)
    val bank_en = WireInit(UIntToOH(offset))
    dontTouch(bank_en) // dontTouch here to disable CIRCT optimizations for it
    out.zipWithIndex.foreach { case (bank, i) =>
      bank.enable := in.enable && bank_en(i)
      bank.set := index
    }
    val bank_en_reg = RegEnable(bank_en, in.enable)
    in.data := Mux1H(bank_en_reg, out.map(_.data))
    in.miss := Mux1H(bank_en_reg, out.map(_.miss))
  }

  def write[T <: CacheSRAMWriteIO[_]](in: T, out: Seq[T]): Unit = {
    val (index, offset) = parseAddr(in.set)
    val bank_en = WireInit(UIntToOH(offset))
    dontTouch(bank_en) // dontTouch here to disable CIRCT optimizations for it
    out.zipWithIndex.foreach { case (bank, i) =>
      bank.enable := in.enable && bank_en(i)
      bank.set := index
      bank.data := in.data
      if (bank.mask.isDefined) {
        bank.mask.get := in.mask.get
      }
    }
  }

  for (i <- 0 until metaRead) {
    read(io.in.meta.read(i), io.out.map(_.meta.read(i)))
  }
  for (i <- 0 until metaWrite) {
    write(io.in.meta.write(i), io.out.map(_.meta.write(i)))
  }
  for (i <- 0 until p.nWays) {
    for (j <- 0 until dataRead) {
      read(io.in.data.read(j)(i), io.out.map(o => o.data.read(j)(i)))
    }
    for (j <- 0 until dataWrite) {
      write(io.in.data.write(j)(i), io.out.map(o => o.data.write(j)(i)))
    }
  }
}

class SRAMReadReduction[T <: CacheSRAMReadIO[_]](gen: T, numIn: Int, numOut: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(numIn, gen)
    val out = Vec(numOut, Flipped(gen))
  })

  io.out.foreach(_.enable := false.B)
  io.out.foreach(_.set := DontCare)

  val req_count = 0.U +: io.in.indices.map(i => PopCount(io.in.take(i + 1).map(_.enable)))
  val req_sel = Seq.fill(io.in.length)(Reg(UInt(numOut.W)))
  for ((read_o, index_o) <- io.out.zipWithIndex) {
    for ((read_i, index_i) <- io.in.zipWithIndex.reverse) {
      if (index_i < index_o) {
        // This entry is impossible to be selected
      } else {
        val last_is_N_minus_1 = req_count(index_i) === index_o.U
        when(read_i.enable && last_is_N_minus_1) {
          req_sel(index_i) := (1 << index_o).U
          read_o.set := read_i.set
        }
      }
    }
    read_o.enable := req_count.last > index_o.U
  }
  io.in.zip(req_sel).foreach { case (i, sel) => i.data := Mux1H(sel.asBools, io.out.map(_.data)) }
  io.in.foreach(_.miss := false.B) // this condition is checked outside this module
}

class SRAMWriteBuffer[T <: CacheSRAMWriteIO[_]](gen: T, numIn: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(numIn, Input(gen))
    val out = Output(gen)
    val state = Output(Valid(gen))
    val conflict = Output(Bool())
  })
  // single-entry buffer
  val write_valid_reg = RegInit(false.B)
  val write_reg = Reg(Output(gen))

  // select the outbound request
  io.out.enable := write_valid_reg || VecInit(io.in.map(_.enable)).asUInt.orR
  io.out.set := Mux(write_valid_reg, write_reg.set, PriorityMux(io.in.map(_.enable), io.in.map(_.set)))

  // incoming requests may be merged with the outbound request (for mask and data)
  val in_set_match = io.in.map(_.set === io.out.set)
  val (outMask, outData) = io.in
    .zip(in_set_match)
    .map { case (req, is_match) =>
      val mask = Mux(req.enable && is_match, req.mask.get, 0.U)
      val maskWidth = req.mask.get.getWidth
      val dataWidth = req.data.getWidth / maskWidth
      val data = req.mask.get.asBools.zip(req.data.asTypeOf(Vec(maskWidth, UInt(dataWidth.W)))).map { case (m, d) =>
        Mux(req.enable && is_match && m, d, 0.U)
      }
      (mask, VecInit(data).asUInt)
    }
    .unzip
  io.out.mask.get := outMask.reduce(_ | _) | Mux(write_valid_reg, write_reg.mask.get, 0.U)
  io.out.data := outData.reduce(_ | _) | Mux(write_valid_reg, write_reg.data, 0.U)

  // buffer update
  val need_buffer = io.in.zip(in_set_match).map(p => p._1.enable && !p._2)
  when(VecInit(need_buffer).asUInt.orR) {
    write_valid_reg := true.B
    // simply OR all requests together, merging multiple requests with the same set
    // we will check for the validity of the merging in the next clock cycle
    write_reg.set := need_buffer.zip(io.in).map { case (v, in) => Mux(v, in.set, 0.U) }.reduce(_ | _)
    write_reg.data := need_buffer.zip(io.in).map { case (v, in) => Mux(v, in.data, 0.U) }.reduce(_ | _)
    write_reg.mask.get := need_buffer.zip(io.in).map { case (v, in) => Mux(v, in.mask.get, 0.U) }.reduce(_ | _)
  }.otherwise {
    write_valid_reg := false.B
  }

  io.state.valid := write_valid_reg
  io.state.bits := write_reg

  // for timing considerations, delay the checker for one clock cycle
  val conflicts = io.in.zip(need_buffer).map { case (in, v) =>
    val need_buffer_reg = RegNext(v, false.B)
    val write_set_reg = RegEnable(in.set, v)
    // We check the equivalence between each write address and the write buffer.
    // If all are equal, the merging at the previous clock cycle is valid.
    !need_buffer_reg || write_set_reg === write_reg.set
  }
  io.conflict := !VecInit(conflicts).asUInt.andR
}

@instantiable
class ReduceSRAMPorts(params: (Int, Int, Int, Int))(implicit p: CacheParams) extends Module {

  val (metaRead, metaWrite, dataRead, dataWrite) = params
  val numMetaReadPorts = if (p.numSRAMReadPorts == 0) metaRead else Seq(metaRead, p.numSRAMReadPorts).min
  val numDataReadPorts = if (p.numSRAMReadPorts == 0) dataRead else Seq(dataRead, p.numSRAMReadPorts).min

  @public val io = IO(new Bundle {
    val in = new Bundle {
      val meta = new Bundle {
        val read = Vec(metaRead, Flipped(new CacheMetaSRAMReadIO))
        val write = Vec(metaWrite, Flipped(new CacheMetaSRAMWriteIO))
        val conflict = Output(Bool())
      }
      val data = new Bundle {
        val read = Vec(dataRead, Vec(p.nWays, Flipped(new CacheDataSRAMReadIO)))
        val write = Vec(dataWrite, Vec(p.nWays, Flipped(new CacheDataSRAMWriteIO)))
        val conflict = Output(UInt(p.nWays.W))
      }
    }
    val out = new Bundle {
      val meta = new Bundle {
        val read = Vec(numMetaReadPorts, new CacheMetaSRAMReadIO)
        val write = new CacheMetaSRAMWriteIO
      }
      val data = new Bundle {
        val read = Vec(numDataReadPorts, Vec(p.nWays, new CacheDataSRAMReadIO))
        val write = Vec(p.nWays, new CacheDataSRAMWriteIO)
      }
    }
  })

  private def merge_readN[T <: Data](
    in: Seq[CacheSRAMReadIO[T]],
    out: Seq[CacheSRAMReadIO[T]],
    buffer: Valid[CacheSRAMWriteIO[T]],
  )(implicit evidence: T <:< Vec[_]): Unit = {
    val resp = if (out.length >= in.length) {
      out.zip(in).foreach { case (o, i) => o <> i }
      out.drop(in.length).foreach(o => o := DontCare)
      out.take(in.length)
    } else {
      val reduction = Module(new SRAMReadReduction(chiselTypeOf(in.head), in.length, out.length))
      reduction.io.in <> in
      reduction.io.out <> out
      for (i <- out.length until in.length) {
        val conflict_cond = in(i).enable && PopCount(in.take(i).map(_.enable)) >= out.length.U
        in(i).miss := RegNext(conflict_cond, false.B)
      }
      reduction.io.in
    }

    // bypass write_reg to read
    in.zip(resp).foreach { case (r, resp) =>
      val is_match = r.enable && buffer.valid && buffer.bits.set === r.set
      val is_full_match = is_match && buffer.bits.mask.get.andR
      // full data match: not miss
      when(RegNext(is_full_match, false.B)) {
        r.miss := false.B
      }
      val slice = (d: UInt, i: Int) => d.asTypeOf(Vec(buffer.bits.mask.get.getWidth, UInt(in.head.elementWidth.W)))(i)
      val data = (0 until buffer.bits.mask.get.getWidth).map { i =>
        val bypass_valid_reg = RegNext(is_match && buffer.bits.mask.get(i), false.B)
        val bypass_data_reg = RegEnable(slice(buffer.bits.data, i), is_match && buffer.bits.mask.get(i))
        Mux(bypass_valid_reg, bypass_data_reg, slice(resp.data, i))
      }
      r.data := VecInit(data).asUInt
    }
  }

  // meta write
  val metaBuffer = Module(new SRAMWriteBuffer(new CacheMetaSRAMWriteIO, metaWrite))
  metaBuffer.io.in <> io.in.meta.write
  metaBuffer.io.out <> io.out.meta.write
  io.in.meta.conflict := metaBuffer.io.conflict
  // meta read
  merge_readN(io.in.meta.read, io.out.meta.read, metaBuffer.io.state)

  val dataBuffers = Seq.fill(p.nWays)(Module(new SRAMWriteBuffer(new CacheDataSRAMWriteIO, dataWrite)))
  for ((dataBuffer, w) <- dataBuffers.zipWithIndex) {
    // data write
    dataBuffer.io.in <> io.in.data.write.map(_(w))
    dataBuffer.io.out <> io.out.data.write(w)
    // data read
    merge_readN(io.in.data.read.map(_(w)), io.out.data.read.map(_(w)), dataBuffer.io.state)
  }
  io.in.data.conflict := VecInit(dataBuffers.map(_.io.conflict)).asUInt
}

@instantiable
class CachedSRAMPort[T <: CacheSRAMReadIO[_], TT <: CacheSRAMWriteIO[_]](gen: T, writeReqs: Seq[TT]) extends Module {
  @public val io = IO(new Bundle {
    val in = Flipped(Flipped(gen))
    val out = Flipped(gen)
    val write = Vec(writeReqs.length, Input(chiselTypeOf(writeReqs.head)))
  })

  val cache_valid = RegInit(false.B)
  val cache = Reg(Output(chiselTypeOf(io.out)))

  // cache match
  val is_read_match = cache_valid && cache.set === io.in.set
  io.out.enable := io.in.enable && !is_read_match
  io.out.set := io.in.set

  // bypass cache data at the next clock cycle
  val is_read_match_reg = RegEnable(is_read_match, io.in.enable)
  val cache_data_width = io.out.data.getWidth
  val num_ways = io.write.head.mask.get.getWidth
  val cache_data_reg = Reg(Vec(num_ways, UInt((cache_data_width / num_ways).W)))
  when(io.in.enable && is_read_match) {
    cache_data_reg := cache.data.asTypeOf(cache_data_reg)
  }

  io.in.data := Mux(is_read_match_reg, cache_data_reg.asTypeOf(io.out.data), io.out.data)
  io.in.miss := RegNext(io.out.enable, false.B) && io.out.miss

  // update or invalidate the cache
  val update_valid = RegNext(io.out.enable, false.B) && !io.out.miss
  // when this cycle has a write, do not update even if they do not match
  val has_write = VecInit(io.write.map(_.enable)).asUInt.orR
  when(update_valid && !has_write) {
    cache_valid := true.B
    cache.set := RegEnable(io.in.set, io.out.enable)
    cache.data := io.in.data
  }

  // invalidate the cache when write
  val is_write_match = VecInit(io.write.map(w => w.enable && w.set === cache.set))
  when(is_write_match.asUInt.orR) {
    cache_valid := false.B
  }

  // may need to bypass write data here
  for (i <- 0 until num_ways) {
    val bypass_valid = io.write.zip(is_write_match).map { case (w, m) => m && w.mask.get(i) }
    val bypass_data_vec = io.write.map(_.data.asTypeOf(cache_data_reg)(i))
    val bypass_data = Mux1H(bypass_valid, bypass_data_vec)
    when(VecInit(bypass_valid).asUInt.orR) {
      cache_data_reg(i) := bypass_data
    }
  }
}

object CachedSRAMPort {
  private val defs = scala.collection.mutable.Map.empty[(String, Int), Definition[CachedSRAMPort[_, _]]]

  def apply[T <: CacheSRAMReadIO[_], TT <: CacheSRAMWriteIO[_]](in: T, write: Seq[TT]): T = {
    val key = (in.getClass.getName, write.length)
    val modDef = defs.getOrElseUpdate(key, Definition(new CachedSRAMPort(chiselTypeOf(in), write)))
    val cache = Instance(modDef).asInstanceOf[Instance[CachedSRAMPort[T, TT]]]
    cache.io.in <> in
    cache.io.write := write
    cache.io.out
  }
}

class CachedSRAMPorts(metaRead: Int, metaWrite: Int, dataRead: Int, dataWrite: Int)(implicit p: CacheParams)
  extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val meta = new Bundle {
        val read = Vec(metaRead, Flipped(new CacheMetaSRAMReadIO))
        val write = Vec(metaWrite, Flipped(new CacheMetaSRAMWriteIO))
      }
      val data = new Bundle {
        val read = Vec(dataRead, Vec(p.nWays, Flipped(new CacheDataSRAMReadIO)))
        val write = Vec(dataWrite, Vec(p.nWays, Flipped(new CacheDataSRAMWriteIO)))
      }
    }
    val out = new Bundle {
      val meta = new Bundle {
        val read = Vec(metaRead, new CacheMetaSRAMReadIO)
      }
      val data = new Bundle {
        val read = Vec(dataRead, Vec(p.nWays, new CacheDataSRAMReadIO))
      }
    }
  })

  for (((in, out), i) <- io.in.meta.read.zip(io.out.meta.read).zipWithIndex) {
    // do not cache the first port
    if (i == 0) {
      out <> in
    } else {
      out <> CachedSRAMPort(in, io.in.meta.write)
      PerfCounter(in.enable, s"meta_read_req_$i")
      PerfCounter(in.enable && !out.enable, s"meta_read_req_cached_$i")
    }
  }

  for (i <- 0 until p.nWays) {
    // do not cache the first port
    for (((in, out), j) <- io.in.data.read.map(_(i)).zip(io.out.data.read.map(_(i))).zipWithIndex) {
      if (j == 0) {
        out <> in
      } else {
        out <> CachedSRAMPort(in, io.in.data.write.map(_(i)))
        if (i == 0) {
          PerfCounter(in.enable, s"data_read_req_$j")
          PerfCounter(in.enable && !out.enable, s"data_read_req_cached_$j")
        }
      }
    }
  }
}
