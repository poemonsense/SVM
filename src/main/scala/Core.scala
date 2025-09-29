package svm

import chisel3._
import chisel3.util._
import difftest.common.{DifftestMemReadIO, DifftestMemWriteIO}

abstract class PipelineStage(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new PipelineBits))
    val out = ValidIO(new PipelineBits)
  })

  val in_valid = io.in.valid && io.in.bits.uop.valid
  val (in, out) = (io.in.bits.uop.bits, io.out.bits.uop.bits)
}

object PipelineConnect {
  def apply[T <: Data](left: Valid[T], right: Valid[T]): Unit = {
    right.valid := RegNext(left.valid, false.B)
    right.bits := RegEnable(left.bits, left.valid)
  }

  def apply[T <: Data](left: Seq[Valid[T]], right: Seq[Valid[T]]): Unit = {
    left.zip(right).foreach { case (l, r) => apply(l, r) }
  }

  def next[T <: Data](left: Valid[T]): Valid[T] = {
    val right = Wire(chiselTypeOf(left))
    apply(left, right)
    right
  }

  def step(left: Valid[PipelineBits], right: Valid[PipelineBits]): Unit = {
    right.valid := RegNext(left.valid, false.B)
    right.bits.id := RegEnable(left.bits.id, left.valid)
    val next_valids = left.bits.more.map(_.valid)
    val has_next_instr = VecInit(next_valids).asUInt.orR
    right.bits.uop.valid := RegEnable(has_next_instr, left.valid)
    right.bits.uop.bits.flags := 0.U.asTypeOf(right.bits.uop.bits.flags)
    right.bits.uop.bits.flags.exit := RegEnable(left.bits.uop.bits.flags.exit, left.valid)
    right.bits.uop.bits.state := RegEnable(left.bits.uop.bits.state, left.valid)
    val uop_valid = left.valid && has_next_instr
    right.bits.uop.bits.hint := RegEnable(PriorityMux(next_valids, left.bits.more.map(_.bits)), uop_valid)
    right.bits.uop.bits.pipe := DontCare
    right.bits.more.zipWithIndex.foreach { case (more, i) =>
      val is_not_first = if (i == 0) false.B else VecInit(next_valids.take(i)).asUInt.orR
      val is_valid = next_valids(i) && is_not_first
      more.valid := RegEnable(is_valid, left.valid)
      more.bits := RegEnable(left.bits.more(i).bits, is_valid && left.valid)
    }
  }
}

class Core(implicit p: SVMParams) extends Module {
  val io = IO(new Bundle {
    val allow_speculation = Input(Bool())
    val in = Flipped(ValidIO(new PipelineBits))
    val out = ValidIO(new PipelineBits)
    val peripheral = Flipped(new RefBusIO)
  })

  val slots = Seq.fill(p.nCommits) {
    val immu = Module(new IMMU)
    val fetch = Module(new Fetch)
    val lagu = Module(new LAGU)
    val dmmu = Module(new DMMU)
    val load = Module(new LoadUnit)
    val sagu = Module(new SAGU)
    val execute = Module(new Arithmetic)
    val priv = Module(new Privileged)
    val trap = Module(new Trap)
    val commit = Module(new Commit)

    // extra IOs
    load.allow_speculation := io.allow_speculation

    Seq(immu, fetch, lagu, dmmu, load, sagu, execute, priv, trap, commit)
  }
  for ((slot, i) <- slots.zipWithIndex) {
    for ((stage, j) <- slot.zipWithIndex) {
      stage.suggestName(s"slot${i}_stage${j}_${stage.desiredName.toLowerCase}")
    }
  }

  io.out := slots.foldLeft(io.in) { case (in, stages) =>
    PipelineConnect.step(in, stages.head.io.in)
    PipelineConnect(stages.dropRight(1).map(_.io.out), stages.tail.map(_.io.in))
    stages.last.io.out
  }

  val mem_r_stages = slots.flatMap(_.filter(_.isInstanceOf[HasMemReadPort]))
  val mem_w_stages = slots.flatMap(_.filter(_.isInstanceOf[HasMemWritePort]))
  val mem_e_stages = slots.flatMap(_.filter(_.isInstanceOf[HasMemWriteExpectation[_]]))
  val mem_r_ports = mem_r_stages.flatMap(_.asInstanceOf[HasMemReadPort].mem_r)
  val mem_w_ports = mem_w_stages.flatMap(_.asInstanceOf[HasMemWritePort].mem_w)
  val mem_e_ports = mem_e_stages.flatMap(_.asInstanceOf[HasMemWriteExpectation[Data]].expect_w)
  val nMemRead = mem_r_ports.length
  val nMemWrite = mem_w_ports.length
  require(mem_e_ports.length == 2 * p.nCommits)
  val expectations = (0 until p.nCommits).flatMap(i =>
    Seq(
      (p.dmmuExpectDelay(i), mem_e_ports(2 * i).payloadType),
      (p.loadExpectDelay(i), mem_e_ports(2 * i + 1).payloadType),
    )
  )
  val mem = Module(new MemOp(nMemRead, nMemWrite, expectations))
  mem.io.read <> mem_r_ports
  mem.io.write <> mem_w_ports
  mem.io.expect.zip(mem_e_ports).foreach(x => x._1 <> x._2)
  mem.io.peripheral <> io.peripheral

  val memory = IO(new Bundle {
    val read = Vec(nMemRead, Flipped(new CacheReadIO))
    val write = Vec(nMemWrite, Flipped(new DifftestMemWriteIO(1)))
  })
  mem.io.memory <> memory

  val commits = slots.map(_.last.io.out)
  def CommitPerfCounter(cond: Valid[PipelineBits] => Bool, name: String): Unit = {
    PerfCounter(PopCount(commits.map(cond)), name)
  }
  PerfCounter(true.B, "cycles")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid, "core_out")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid && c.bits.uop.bits.flags.is_miss, "core_out_miss")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid && c.bits.uop.bits.flags.miss_immu, "core_out_miss_immu")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid && c.bits.uop.bits.flags.miss_fetch, "core_out_miss_fetch")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid && c.bits.uop.bits.flags.miss_dmmu, "core_out_miss_dmmu")
  CommitPerfCounter(c => c.valid && c.bits.uop.valid && c.bits.uop.bits.flags.miss_load, "core_out_miss_load")
  printf("Assertion failed:")
}
