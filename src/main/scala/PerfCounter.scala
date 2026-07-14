package svm

import chisel3._
import chisel3.util.experimental.BoringUtils
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object PerfCounter {
  private val sources = scala.collection.mutable.ListBuffer.empty[(UInt, String)]
  private val sinks = scala.collection.mutable.ListBuffer.empty[UInt]
  private var enabled = true

  def configure(enable: Boolean): Unit = {
    enabled = enable
    sources.clear()
    sinks.clear()
  }

  private def wiring_name(i: Int): String = s"svm_perf_$i"
  private def do_pair(src: UInt, dest: UInt, index: Int) = {
    val name = wiring_name(index)
    BoringUtils.addSource(src, name)
    BoringUtils.addSink(dest, name)
  }

  def apply(inc: UInt, name: String): UInt = {
    if (!enabled) {
      return 0.U(64.W)
    }
    val cycleCnt = RegInit(0.U(64.W))
    cycleCnt := cycleCnt + 1.U
    val counter = RegInit(0.U(64.W))
    counter := counter + inc
    // 1024 cycles
    when ((cycleCnt & 0x3ff.U) === 0x3ff.U) {
      printf(p"[$cycleCnt] $name: $counter\n")
    }
    val inc_reg = RegNext(inc, 0.U(64.W))
    sources.append((inc_reg, name))
    if (sources.length <= sinks.length) {
      do_pair(inc_reg, sinks(sources.length - 1), sources.length - 1)
    }
    inc_reg
  }

  def apply(inc: UInt, name: String, maxVal: Int): Unit = {
    apply(inc, name)
    for (i <- 0 to maxVal) {
      apply(WireInit(inc === i.U), s"${name}_$i")
    }
  }

  def sink(inc: UInt): UInt = {
    if (!enabled) {
      return 0.U(64.W)
    }
    sinks.append(inc)
    if (sinks.length <= sources.length) {
      do_pair(sources(sinks.length - 1)._1, inc, sinks.length - 1)
    }
    inc
  }

  def collect(filename: String, n: Int): Unit = {
    val headerCounters = if (enabled) n else 0
    val numCounters = if (enabled) Seq(sources.length, sinks.length, n).min else 0
    val names = if (enabled) sources.take(numCounters).map(_._2) else Seq.empty
    val c_array = names.map(n => s"\"$n\"") ++ Seq.fill((headerCounters - numCounters).max(1))("NULL")
    val c_str = Seq(
      "#ifndef SVM_PERF_COUNTERS",
      s"#define SVM_PERF_COUNTERS $headerCounters",
      s"#define SVM_ENABLE_PERF_COUNTERS ${if (enabled) 1 else 0}",
      "#include <stddef.h>",
      "static const char *perf_names[] = {",
      c_array.mkString(",\n"),
      "};",
      "#endif",
    ).mkString("\n")
    Files.write(Paths.get(filename), c_str.getBytes(StandardCharsets.UTF_8))
  }
}
