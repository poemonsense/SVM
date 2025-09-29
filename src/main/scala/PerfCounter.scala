package svm

import chisel3._
import chisel3.util.experimental.BoringUtils
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object PerfCounter {
  private val sources = scala.collection.mutable.ListBuffer.empty[(UInt, String)]
  private val sinks = scala.collection.mutable.ListBuffer.empty[UInt]

  private def wiring_name(i: Int): String = s"svm_perf_$i"
  private def do_pair(src: UInt, dest: UInt, index: Int) = {
    val name = wiring_name(index)
    BoringUtils.addSource(src, name)
    BoringUtils.addSink(dest, name)
  }

  def apply(inc: UInt, name: String): UInt = {
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
    sinks.append(inc)
    if (sinks.length <= sources.length) {
      do_pair(sources(sinks.length - 1)._1, inc, sinks.length - 1)
    }
    inc
  }

  def collect(filename: String, n: Int): Unit = {
    val numCounters = Seq(sources.length, sinks.length, n).min
    val names = sources.take(numCounters).map(_._2)
    val c_array = names.map(n => s"\"$n\"") ++ Seq.fill(n - numCounters)("NULL")
    val c_str = Seq(
      "#ifndef SVM_PERF_COUNTERS",
      s"#define SVM_PERF_COUNTERS $n",
      "#include <stddef.h>",
      "static const char *perf_names[] = {",
      c_array.mkString(",\n"),
      "};",
      "#endif",
    ).mkString("\n")
    Files.write(Paths.get(filename), c_str.getBytes(StandardCharsets.UTF_8))
  }
}
