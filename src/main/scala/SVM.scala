package svm

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.{DifftestMem, DifftestMemReadIO, DifftestMemWriteIO}
import svm.util.VerificationExtractor
import svm.definitions.RVMicroOp

class GoldenState extends Bundle {
  val regs_int = new ArchIntRegState
  val csr = new FullCSRState
  val regs_fp = new ArchFpRegState
}

class GoldenStep(nCommits: Int) extends GoldenState {
  val uop = Vec(nCommits, Valid(new RVMicroOp))
}

class SVM(implicit p: SVMParams) extends Module {
  val dut = IO(new Bundle {
    val reset = Output(Bool())
    val step = Input(Valid(new GoldenStep(p.nCommits)))
    val mem = Input(new Bundle {
      val read = new DifftestMemReadIO(p.memBeatSize)
      val write = new DifftestMemWriteIO(p.memBeatSize)
    })
  })

  val golden = dut.step
  val tracker = Module(new StateTracker(chiselTypeOf(golden.bits)))

  dut.reset := tracker.io.dut_reset

  tracker.io.step := golden
  tracker.io.dut_exit := false.B

  val core = Module(new Core)
  core.reset := reset.asBool || tracker.io.core_reset
  core.io.in := tracker.io.execute
  core.io.allow_speculation := tracker.io.allow_speculation
  tracker.io.commit := core.io.out
  core.io.peripheral <> tracker.io.rw

  val dut_mem = dut.mem
  assert(!dut_mem.read.valid || !dut_mem.write.valid, "support single port only")
  assert(!dut_mem.write.valid || dut_mem.write.mask.asUInt.andR, "support full mask only in write")

  if (p.withCache.isDefined) {
    val cacheParams = CacheParams(
      p.withCache.get,
      p.cacheWays,
      nWords = p.memBeatSize,
      physicalAddressSpace = p.paddrBits,
      refillOnReadMiss = p.cacheRefillOnReadMiss,
      nRead = core.nMemRead,
      nWrite = core.nMemWrite,
      useSimpleSRAMs = p.isFPGA,
      numSRAMBanks = p.cacheBanks,
      numSRAMReadPorts = p.cacheSRAMPorts,
      replacement = p.cacheReplacement,
    )
    println(s"REF Cache: ${cacheParams.cacheSize / 1024} KB, ${cacheParams.nRead}R${cacheParams.nWrite}W")
    println(s" nSets: ${cacheParams.nSets}")
    println(s" nWays: ${cacheParams.nWays}")
    println(s" refillOnReadMiss: ${cacheParams.refillOnReadMiss}")
    println(s" numSRAMBanks: ${cacheParams.numSRAMBanks}")
    println(s" numSRAMReadPorts: ${cacheParams.numSRAMReadPorts}")
    println(s" replacement: ${cacheParams.replacement}")
    val cache = Module(new Cache()(cacheParams))
    cache.reset := reset.asBool || tracker.io.core_reset
    cache.io.read <> core.memory.read
    cache.io.write <> core.memory.write
    cache.io.dut.valid := dut_mem.read.valid || dut_mem.write.valid
    cache.io.dut.bits.addr := Mux(dut_mem.read.valid, dut_mem.read.index, dut_mem.write.index)
    cache.io.dut.bits.wen := dut_mem.write.valid
    cache.io.dut.bits.data := Mux(dut_mem.write.valid, dut_mem.write.data, dut_mem.read.data)

    when(!cache.io.reset_done) {
      dut.reset := true.B
    }
  } else {
    val mem = DifftestMem(p.memSize, 8, 8, core.memory.read.length, core.memory.write.length)
    for (read <- core.memory.read) {
      val read_index = read.index - (p.memBase >> 3).U
      val addr_fix = if (p.isPalladium) read_index else read_index + (p.memSize >> 3).U
      read.data := mem.read(addr_fix, read.valid) // auto increment for ports
      read.miss := false.B
    }
    for ((write, i) <- core.memory.write.zipWithIndex) {
      val write_index = write.index - (p.memBase >> 3).U
      val addr_fix = if (p.isPalladium) write_index else write_index + (p.memSize >> 3).U
      mem.write(i) := write
      mem.write(i).index := addr_fix
    }
  }

  val ref = IO(new Bundle {
    val state = Output(UInt(64.W))
    val uart = Output(Valid(UInt(8.W)))
  })
  ref.state := tracker.io.state
  ref.uart.valid := tracker.io.uart.out.valid
  ref.uart.bits := tracker.io.uart.out.ch
  tracker.io.uart.in.ch := DontCare

  PerfCounter.collect("bootrom/generated-perf.h", tracker.state_recorder.perf_counter_reg.length)
  VerificationExtractor.cppHeader = Some("bootrom/generated-assertion.h")
}
