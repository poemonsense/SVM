package svm.definitions

import chisel3._
import chisel3.util._
import svm._

import scala.meta.{Stat, Term}
import scala.meta.parsers._
import scala.util.matching.Regex.Match

object Spike {
  private val encodingCache = scala.collection.mutable.Map[String, BigInt](
    "MASK_ORC_B" -> BigInt(0xfff0707fL),
    "MATCH_ORC_B" -> BigInt(0x28705013L),
    "MASK_REV8" -> BigInt(0xfff0707fL),
    "MATCH_REV8" -> BigInt(0x6b805013L),
    "MASK_ZEXT_H" -> BigInt(0xfff0707fL),
    "MATCH_ZEXT_H" -> BigInt(0x0800403bL),
  )
  def getEncoding(target: String): BigInt = {
    encodingCache.getOrElseUpdate(target, eval(getEncodingString(target.toUpperCase)))
  }

  def getInstructionTemplate(instr: String): Seq[String] = {
    val lines = getInstructionTemplateFile(instr.dotAsUnderscore)
    // Some C++ lines require format fixes
    val removeIfElseSemicolon = lines.zipWithIndex.map { case (l, i) =>
      val prevIsIf = i > 0 && (lines(i - 1).strip().startsWith("if") || lines(i - 1).strip().startsWith("else if"))
      val nextIsElse = i < lines.length - 1 && lines(i + 1).strip().startsWith("else")
      if (prevIsIf && nextIsElse && l.strip().endsWith(";")) l.replaceAll(";", " ") else l
    }
    val lambdaRepl = (m: Match) => m.group(4).replace(m.group(3), s"${m.group(1)}(amo_load_data)")
    val lineReplacement = Seq(
      // [&](int8_t lhs) { return lhs + RS2; } => int8_t(amo_load_data) + RS2
      (raw"\[&\]\s*\((\w+)\s+(\w+\s+|)(\w+)\)\s*\{\s*return\s+([^;}\n]+);\s*}".r, lambdaRepl)
    )
    val scalaLines = removeIfElseSemicolon.map(l =>
      lineReplacement.foldLeft(l) { case (l, (re, repl)) =>
        re.replaceAllIn(l, repl)
      }
    )
    // Split line into expressions
    val rawExpr = scalaLines.mkString(" ").split(";").filter(_.strip().nonEmpty).toSeq
    // Some C++ expressions require format fixes
    val cppTemplateRepl = (m: Match) => s"(${m.group(1)})"
    val cppCastRepl = (m: Match) =>
      m.group(1).replace("(", "").replace(")", "(") + m.group(2) + ")" * m.group(1).count(_ == '(')
    val exprReplacement = Seq(
      // (\w+)+\w+: (uint64_t)(uint32_t)RS1 => uint64_t(uint32_t(RS1))
      (raw"((?:\(\w+\))+)(\w+|\(\w+\))".r, cppCastRepl),
      // <\w+>: <uint64_t>, <int64_t> => (uint64_t), (int64_t)
      (raw"<(\w+)>".r, cppTemplateRepl),
      // std::min => std.min
      (raw"::".r, (_: Match) => "."),
      // std::min => std.min
      (raw"\d+LL ".r, (m: Match) => m.toString().replace("LL", "L")),
      // remove all require_extension lines
      (raw"require_(either_|)extension\(.*\)".r, (_: Match) => ""),
    )
    // Format every expression
    val expr = rawExpr.map(exprReplacement.foldLeft(_) { case (ex, (re, repl)) => re.replaceAllIn(ex, repl) })
    expr.map(_.strip()).filter(_.nonEmpty)
  }

  def csrNames: Seq[String] = encoding.filter(_.startsWith("#define CSR_")).map(_.split(" ")(1).replace("CSR_", ""))

  def isHardfloatOp(op: String): Boolean = {
    val func = Seq("fsgnj32", "fsgnj64", "i32_to_f32", "ui32_to_f32", "i64_to_f32", "ui64_to_f32", "i32_to_f64",
      "ui32_to_f64", "i64_to_f64", "ui64_to_f64")
    Seq("f32_", "f64_").exists(op.startsWith) || func.contains(op)
  }

  // Some instructions are hard-decoded here. The Spike code is difficult to interpret.
  private def rs1(uop: MicroOp): UInt = uop.state.xpr(uop.rv.rs1)
  private def rs2(uop: MicroOp): UInt = uop.state.xpr(uop.rv.rs2)
  private def clz(uop: MicroOp, range: Int): UInt = PriorityEncoder(rs1(uop)(range - 1, 0).asBools.reverse :+ true.B)
  def clz(uop: MicroOp): UInt = clz(uop, 64)
  def clzw(uop: MicroOp): UInt = clz(uop, 32)
  private def ctz(uop: MicroOp, range: Int): UInt = PriorityEncoder(rs1(uop)(range - 1, 0).asBools :+ true.B)
  def ctz(uop: MicroOp): UInt = ctz(uop, 64)
  def ctzw(uop: MicroOp): UInt = ctz(uop, 32)
  private def cpop(uop: MicroOp, range: Int): UInt = PopCount(rs1(uop)(range - 1, 0))
  def cpop(uop: MicroOp): UInt = cpop(uop, 64)
  def cpopw(uop: MicroOp): UInt = cpop(uop, 32)
  def orc_b(uop: MicroOp): UInt = VecInit(rs1(uop).asTypeOf(Vec(8, UInt(8.W))).map(b => Fill(8, b.orR))).asUInt
  def rev8(uop: MicroOp): UInt = VecInit(rs1(uop).asTypeOf(Vec(8, UInt(8.W))).reverse).asUInt
  def zexth(uop: MicroOp): UInt = ZeroExt(rs1(uop)(15, 0), 64)
  private def clmul(rs1: UInt, rs2: UInt, width: Int): Seq[Bool] = {
    (0 until 2 * width - 1).map(i => {
      val (lo, hi) = (Seq(0, i - (width - 1)).max, Seq(i, width - 1).min)
      (lo to hi).map(j => rs1(j) && rs2(i - j)).reduce(_ ^ _)
    })
  }
  def clmul(uop: MicroOp): UInt = VecInit(clmul(rs1(uop), rs2(uop), 64).take(64)).asUInt
  def clmulh(uop: MicroOp): UInt = VecInit(clmul(rs1(uop), rs2(uop), 64).drop(64)).asUInt
  def clmulr(uop: MicroOp): UInt = {
    val rs1r = VecInit(rs1(uop).asBools.reverse).asUInt
    val rs2r = VecInit(rs2(uop).asBools.reverse).asUInt
    VecInit(clmul(rs1r, rs2r, 64).take(64).reverse).asUInt
  }

  private val includingPath = scala.reflect.io.File("riscv").toAbsolute.toString()
  private val encodingFile = scala.io.Source.fromFile(s"$includingPath/encoding.h")
  private val encoding = encodingFile.getLines().toSeq
  encodingFile.close()
  private def insnTemplateFile(insn: String): String = s"$includingPath/insns/${insn.toLowerCase}.h"

  private def getEncodingString(macro_name: String): String = {
    val line = encoding.find(_.startsWith(s"#define $macro_name "))
    val noCommentLine = line.map(l => raw"/\*.*\*/".r.replaceAllIn(l, (_: Match) => ""))
    val removeDefine = noCommentLine.map(_.strip().split(' ').filter(_.nonEmpty).drop(2).mkString(" "))
    removeDefine.getOrElse {
      assert(line.isDefined, s"encoding of $macro_name is not found")
      ""
    }
  }

  private def eval(expr: String): BigInt = {
    if (expr.strip().contains(" ")) {
      expr.parse[Stat].get match {
        case term: Term.ApplyInfix =>
          term.op.value match {
            case "+"  => eval(term.lhs.toString()) + eval(term.argClause.toString())
            case "|"  => eval(term.lhs.toString()) | eval(term.argClause.toString())
            case "<<" => eval(term.lhs.toString()) << eval(term.argClause.toString()).toInt
          }
      }
    } else if (expr.startsWith("0x")) {
      BigInt(expr.substring(2), 16)
    } else if (expr.forall(_.isDigit)) {
      BigInt(expr)
    } else {
      getEncoding(expr)
    }
  }

  private def getInstructionTemplateFile(insn: String): Seq[String] = {
    val source = scala.io.Source.fromFile(insnTemplateFile(insn))
    val lines = source.getLines().map(_.strip()).filterNot(_.startsWith("//")).toSeq
    source.close()
    lines
  }
}
