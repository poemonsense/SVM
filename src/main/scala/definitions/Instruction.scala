package svm.definitions

import chisel3._
import chisel3.util._
import svm._

import scala.language.implicitConversions
import scala.meta.{Lit, Stat, Term}
import scala.meta.parsers._
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

abstract class Instruction {
  // Check the existence of a variable named `x`
  def contains(x: String): Boolean = false
  // Retrieve the optional variable named `x`
  def retrieve(x: String): Option[Value] = None

  // Flatten all instructions (to eliminate Block recursion)
  def flatten: Seq[Instruction] = Seq(this)
}

case class StateTransition(tpe: String, cond: Option[BValue], value: Option[Value], msg: Option[String])
  extends Instruction {
  def when(whenCond: BValue): StateTransition = {
    val newCond = if (tpe == "assert") {
      cond.map(c => BValue(uop => whenCond.not.B(uop) || c.B(uop))).getOrElse(whenCond.not)
    } else {
      cond.map(c => BValue(uop => whenCond.B(uop) && c.B(uop))).getOrElse(whenCond)
    }
    StateTransition(tpe, Some(newCond), value, msg)
  }

  def when(whenCond: Bool): StateTransition = when(BValue(_ => whenCond))
}

object StateTransition {
  def assert(cond: BValue, msg: String) = StateTransition("assert", Some(cond), None, Some(msg))
  def redirectPC(pc: Value) = StateTransition("npc", None, Some(pc), None)
  def xprWrite(data: Value) = StateTransition("xpr", None, Some(data), None)
  def fprWrite(data: Value) = StateTransition("fpr", None, Some(data), None)
  def setRM(data: Value) = StateTransition("rm", None, Some(data), None)
  def setHardfloat(op: String) = StateTransition("hardfloat", None, None, Some(op))
  def setFflags() = StateTransition("fflags", None, None, None)
}

case class Load(address: Value, opcode: Value) extends Instruction
case class Store(address: Value, opcode: Value, data: Value) extends Instruction
case class AMO(address: Value, opcode: Value, data: Value) extends Instruction

case class When(cond: BValue, thenp: Instruction, elsep: Instruction) extends Instruction

case class NamedValue(name: String, value: Value) extends Instruction {
  override def contains(x: String): Boolean = name == x
  override def retrieve(x: String): Option[Value] = Option.when(name == x)(value)
}

case class Block(operations: Seq[Instruction] = Seq()) extends Instruction {
  override def contains(x: String): Boolean = operations.exists(_.contains(x))
  override def retrieve(x: String): Option[Value] = operations.find(_.contains(x)).map(_.retrieve(x).get)

  override def flatten: Seq[Instruction] = operations.flatMap(_.flatten)
}

object ValueTypes extends Enumeration {
  type ValueType = this.Value
  val UIntT, SIntT, F32T, F64T = this.Value
}
import ValueTypes._

abstract class Value(val tpe: ValueType, val value: MicroOp => UInt) {
  def signed: Boolean = tpe == SIntT

  def U(uop: MicroOp): UInt = value(uop)
  def S(uop: MicroOp): SInt = U(uop).asSInt
  def B(uop: MicroOp): Bool = U(uop).asBool
  def asData(uop: MicroOp): Data = if (signed) S(uop) else U(uop)

  def asUInt: Value = UValue(value)
  def asBool: BValue = BValue(uop => value(uop).asBool)

  def do_binaryOp(op: String, that: Value): Value = {
    // TODO: this is a temp fix for logic operators
    val logic_ops = Seq(">=", ">", "<=", "<")
    val s = signed && !logic_ops.contains(op)
    Value(s, uop => methodCall(s"do_${op.flatMap(encodeOperator)}", asData(uop), that.asData(uop)))
  }

  def mux(truev: Value, falsev: Value): Value = Value(truev.signed, uop => Mux(B(uop), truev.U(uop), falsev.U(uop)))
  def max(that: Value): Value = do_binaryOp(">=", that).mux(this, that)
  def min(that: Value): Value = do_binaryOp(">=", that).mux(that, this)

  private def encodeOperator(c: Char): String = {
    Map(
      '+' -> "$plus",
      '-' -> "$minus",
      '*' -> "$times",
      '/' -> "$div",
      '%' -> "$percent",
      '<' -> "$less",
      '>' -> "$greater",
      '=' -> "$eq",
      '&' -> "$amp",
      '|' -> "$bar",
      '!' -> "$bang",
      '~' -> "$tilde",
      '^' -> "$up",
    ).getOrElse(c, c.toString)
  }

  private def methodCall(op: String, x: AnyRef, y: AnyRef): UInt = {
    val mirror = currentMirror.reflect(x)
    val methods = mirror.symbol.typeSignature.decls.filter(_.isMethod).map(_.asMethod)
    val operandMatch = (m: MethodSymbol) => m.paramLists.flatten.head.typeSignature.toString == y.getClass.getName
    val method = methods.find(m => m.name.toString == op && operandMatch(m))
    if (method.isDefined) {
      val arguments = method.get.paramLists.flatten.map(_.name.toString).map {
        case "that"       => y
        case "sourceInfo" => chisel3.experimental.SourceInfo.materialize
      }
      mirror.reflectMethod(method.get).apply(arguments: _*).asInstanceOf[Bits].asUInt
    } else {
      // Try to convert x/y to Bool. If failed, just give up and abort
      try {
        val xB = if (x.isInstanceOf[Bool]) x.asInstanceOf[Bool] else x.asInstanceOf[UInt] =/= 0.U
        val yB = if (y.isInstanceOf[Bool]) y.asInstanceOf[Bool] else y.asInstanceOf[UInt] =/= 0.U
        val result = methodCall(op, xB, yB)
        println(s"Reflection: ${x.getClass.getName}.$op(${y.getClass.getName}) is converted to a Bool operator")
        result
      } catch {
        case _: Throwable =>
          throw new IllegalArgumentException(s"No matching $op for ${x.getClass.getName} ${y.getClass.getName}")
      }
    }
  }
}

object Value {
  def apply(signed: Boolean, value: MicroOp => UInt): Value = if (signed) SValue(value) else UValue(value)
}

case class BValue(override val value: MicroOp => Bool) extends Value(UIntT, value) {
  def not: BValue = BValue(uop => !value(uop))
}
case class UValue(_value: MicroOp => UInt) extends Value(UIntT, _value)
case class SValue(_value: MicroOp => UInt) extends Value(SIntT, _value)
case class F32Value(_value: MicroOp => UInt) extends Value(F32T, _value)
case class F64Value(_value: MicroOp => UInt) extends Value(F64T, _value)

case class LoadData() extends Value(UIntT, _.pipe.lsu.bits.load_data)
case class DivData() extends Value(UIntT, _.hint.div_data)
case class RM() extends Value(UIntT, _.pipe.lsu.bits.load_data)
case class HardfloatData() extends Value(UIntT, _.pipe.lsu.bits.load_data)

case class Context(values: Seq[Value], instruction: Instruction) {
  def contains(x: String): Boolean = instruction.contains(x)
  def retrieve(x: String): Option[Value] = instruction.retrieve(x)

  def +(that: Context): Context = Context(values ++ that.values, Block(instruction.flatten ++ that.instruction.flatten))

  // A special case of +. Add that to the beginning of current context
  def push(that: Context): Context = that + this
  def pop(i: Int): (Seq[Value], Context) = (values.take(i), Context(values.drop(2), instruction))
  def pop: (Value, Context) = {
    val (v, c) = pop(1)
    (v.head, c)
  }
}

object Instruction {
  implicit private def valueToContext(v: Value): Context = Context(values = Seq(v), instruction = Block())
  implicit private def instrToContext(i: Instruction): Context = Context(values = Seq(), instruction = i)
  implicit private def contextToInstr(c: Context): Instruction = c.instruction
  implicit private def contextToValue(c: Context): Value = c.values.head
  implicit private def stringToTerm(s: String): Term = s.parse[Stat].get.asInstanceOf[Term]

  private def extractLSUOpcode(s: String): UInt = {
    val pattern = raw"MMU.((store|load|amo)\w*)\((u|)int(\d+)_t\)".r
    require(pattern.matches(s), s"$s not supported")
    val regex = pattern.findFirstMatchIn(s).get
    val is_lrsc = regex.group(1).contains("_")
    val is_load = regex.group(2) == "load" || regex.group(2) == "amo"
    val is_store = regex.group(2) == "store" || regex.group(2) == "amo"
    val is_unsigned = regex.group(3) == "u"
    val req_size = log2Ceil(regex.group(4).toInt / 8)
    LSUOpcode(is_lrsc.B, is_load.B, is_store.B, is_unsigned.B, req_size.U(2.W))
  }

  private def apply(term: Term)(implicit context: Context): Context = {
    term match {
      case _: Lit.Unit => context
      case node: Lit.Int =>
        context.push(if (node.value > 0) UValue(_ => node.value.U) else SValue(_ => node.value.S.asUInt))
      case node: Lit.Long =>
        context.push(if (node.value > 0) UValue(_ => node.value.U) else SValue(_ => node.value.S.asUInt))
      case node: Lit.Boolean =>
        context.push(BValue(_ => node.value.B))
      case node: Term.Name =>
        context.push(node.value match {
          case "require_rv64"           => Block()
          case "xlen"                   => UValue(_ => 64.U)
          case "RS1"                    => UValue(uop => uop.state.xpr(uop.rv.rs1))
          case "RS2"                    => UValue(uop => uop.state.xpr(uop.rv.rs2))
          case "SHAMT"                  => UValue(uop => uop.rv.i_imm() & 0x3f.U)
          case "pc"                     => UValue(_.state.pc)
          case "npc"                    => UValue(_.snpc)
          case "BRANCH_TARGET"          => UValue(uop => uop.state.pc + uop.rv.sb_imm())
          case "JUMP_TARGET"            => UValue(uop => uop.state.pc + uop.rv.uj_imm())
          case "UINT64_MAX"             => UValue(_ => Fill(64, true.B))
          case "INT64_MIN"              => UValue(_ => Cat(true.B, Fill(63, false.B)))
          case "speculative_rd"         => LoadData()
          case "amo_load_data"          => LoadData()
          case "div_data"               => DivData()
          case n if context.contains(n) => context.retrieve(n).get
          case "require_fp"             => StateTransition.assert(BValue(_.state.fsEnabled), "fs disabled")
          case "set_fp_exceptions"      => StateTransition.setFflags()
          case "RM"                     => RM()
          case "FRS1_F"                 => F32Value(uop => uop.state.fpr(uop.rv.rs1))
          case "FRS2_F"                 => F32Value(uop => uop.state.fpr(uop.rv.rs2))
          case "FRS3_F"                 => F32Value(uop => uop.state.fpr(uop.rv.rs3))
          case "FRS1_D"                 => F64Value(uop => uop.state.fpr(uop.rv.rs1))
          case "FRS2_D"                 => F64Value(uop => uop.state.fpr(uop.rv.rs2))
          case "FRS3_D"                 => F64Value(uop => uop.state.fpr(uop.rv.rs3))
          case "F32_SIGN"               => UValue(_ => Cat(true.B, Fill(31, false.B)))
          case "F64_SIGN"               => UValue(_ => Cat(true.B, Fill(63, false.B)))
          case "defaultNaNF32UI"        => UValue(_ => 0x7fc00000.U)
          case "defaultNaNF64UI"        => UValue(_ => Cat(0x7ff8.U(16.W), 0.U(48.W)))
        })
      case node: Term.Apply =>
        val fun = node.fun.toString()
        val zeroOps = fun.startsWith("insn.")
        val binaryOps = Seq("mul", "std.max", "std.min", "MMU.load", "MMU.store", "MMU.amo").exists(fun.startsWith)
        val numOperands = if (binaryOps) 2 else if (zeroOps) 0 else 1
        val (args, newContext) = apply(node.argClause.toString()).pop(numOperands)
        newContext.push(node.fun.toString() match {
          case "reg_t"       => UValue(uop => ZeroExt(args.head.U(uop), 64))
          case "sreg_t"      => SValue(uop => SignExt(args.head.U(uop), 64))
          case "zext32"      => UValue(uop => ZeroExt(ZeroExt(args.head.U(uop), 32), 64))
          case "sext32"      => SValue(uop => SignExt(ZeroExt(args.head.U(uop), 32), 64))
          case "zext_xlen"   => UValue(uop => ZeroExt(args.head.U(uop), 64))
          case "sext_xlen"   => SValue(uop => SignExt(args.head.U(uop), 64))
          case s"uint${w}_t" => UValue(uop => ZeroExt(args.head.U(uop), w.toInt))
          case s"int${w}_t"  => SValue(uop => ZeroExt(args.head.U(uop), w.toInt))
          case "insn.i_imm"  => UValue(_.rv.i_imm())
          case "insn.u_imm"  => UValue(_.rv.u_imm())
          case "insn.s_imm"  => UValue(_.rv.s_imm())
          case "WRITE_RD"    => StateTransition.xprWrite(args.head)
          case "set_pc"      => StateTransition.redirectPC(args.head)
          case "mulh"        => UValue(uop => (args(0).S(uop) * args(1).S(uop))(127, 64))
          case "mulhsu"      => UValue(uop => (args(0).S(uop) * args(1).U(uop))(127, 64))
          case "mulhu"       => UValue(uop => (args(0).U(uop) * args(1).U(uop))(127, 64))
          case x if x == "std.min" || x == "std.max" =>
            if (x.endsWith("min")) args(0).min(args(1)) else args(0).max(args(1))
          case load if load.startsWith("MMU.load") =>
            context.push(
              Context(
                values = Seq(LoadData()),
                instruction = Load(args(0), UValue(_ => extractLSUOpcode(load))),
              )
            )
          case store if store.startsWith("MMU.store") =>
            val sc = Option.when(store.contains("conditional"))(UValue(_.pipe.lsu.bits.sc_success))
            context.push(
              Context(
                values = sc.toSeq,
                instruction = Store(args(0), UValue(_ => extractLSUOpcode(store)), args(1)),
              )
            )
          case amo if amo.startsWith("MMU.amo") =>
            context.push(
              Context(
                values = Seq(LoadData()),
                instruction = AMO(args(0), UValue(_ => extractLSUOpcode(amo)), args(1)),
              )
            )
          case "require" => StateTransition.assert(BValue(uop => args(0).U(uop) =/= 0.U), node.argClause.toString())
          case "assert"  => StateTransition.assert(args.head.asBool, node.argClause.toString())
          case "f32"     => F32Value(uop => ZeroExt(args.head.U(uop), 32))
          case "f64"     => F64Value(uop => ZeroExt(args.head.U(uop), 64))
          case "freg"    => F32Value(uop => SignExt(args.head.U(uop), 64))
          // TODO: fix hardfloat operations and results
          case x if x.startsWith("WRITE_FRD") =>
            val transitions = newContext.instruction.flatten.filter(_.isInstanceOf[StateTransition])
            val hasHardfloat = transitions.exists(_.asInstanceOf[StateTransition].tpe == "hardfloat")
            val value = if (hasHardfloat) HardfloatData() else UValue(uop => OneExt(args.head.U(uop), 64))
            StateTransition.fprWrite(value)
          case x if Spike.isHardfloatOp(x) =>
            Context(
              values = Seq(HardfloatData()),
              instruction = StateTransition.setHardfloat(x),
            )
          case "isNaNF32UI" =>
            BValue(uop => {
              val x = args.head.U(uop)
              x(30, 23).andR && x(22, 0).orR
            })
          case "isNaNF64UI" =>
            BValue(uop => {
              val x = args.head.U(uop)
              x(62, 52).andR && x(51, 0).orR
            })
          case "arith_ctz" =>
            UValue(uop => PriorityEncoder(args(0).U(uop)(args(1).U(uop).litValue - 1, 0)))
          case "arith_clz" =>
            UValue(uop => PriorityEncoder(args(0).U(uop)(args(1).U(uop).litValue - 1, 0).asBools.reverse))
          case "arith_pop" =>
            UValue(uop => PopCount(args(0).U(uop)(args(1).U(uop).litValue - 1, 0)))
        })
      case node: Term.ApplyInfix =>
        val (rhs, newContext1) = apply(node.argClause.toString()).pop
        val (lhs, newContext2) = apply(node.lhs)(newContext1).pop
        newContext2.push(node.op.value match {
          case "=="                               => lhs.asUInt.do_binaryOp("===", rhs.asUInt)
          case "!="                               => lhs.asUInt.do_binaryOp("=/=", rhs.asUInt)
          case op if Seq("<<", ">>").contains(op) => lhs.do_binaryOp(op, UValue(uop => rhs.U(uop)(5, 0)))
          case op                                 => lhs.do_binaryOp(op, rhs)
        })
      case node: Term.ApplyUnary =>
        val (arg, newContext) = apply(node.arg).pop
        newContext.push(node.op.value match {
          case "~" => UValue(uop => (~arg.U(uop)).asUInt)
          case "!" => BValue(uop => !arg.B(uop))
          case "-" => UValue(uop => (~arg.U(uop)).asUInt + 1.U)
        })
      case node: Term.Assign =>
        val lhs_s = node.lhs.toString()
        val (rhs, newContext) = apply(node.rhs).pop
        newContext.push(lhs_s.split(' ').head match {
          case "reg_t" | "bool" | "sreg_t" | "int" => NamedValue(lhs_s.split(' ').last, rhs)
          case "softfloat_roundingMode"            => StateTransition.setRM(rhs)
        })
      case node: Term.If =>
        context.push(When(BValue(apply(node.cond).B), apply(node.thenp), apply(node.elsep)))
      case node: Term.Tuple => node.args.map(arg => apply(arg)).reduceLeft(_ + _)
      case node: Term.ApplyType =>
        context.push(s"${node.fun}${node.argClause}" match {
          case "FRS1.v[0]" => UValue(uop => uop.state.fpr(uop.rv.rs1))
          case "FRS2.v[0]" => UValue(uop => uop.state.fpr(uop.rv.rs2))
        })
      case node: Term.Select =>
        context.push(s"${node.qual}.${node.name}" match {
          case "FRS1_F.v" => UValue(uop => uop.state.fpr(uop.rv.rs1)(31, 0))
          case "FRS2_F.v" => UValue(uop => uop.state.fpr(uop.rv.rs2)(31, 0))
          case "FRS3_F.v" => UValue(uop => uop.state.fpr(uop.rv.rs3)(31, 0))
          case "FRS1_D.v" => UValue(uop => uop.state.fpr(uop.rv.rs1))
          case "FRS2_D.v" => UValue(uop => uop.state.fpr(uop.rv.rs2))
          case "FRS3_D.v" => UValue(uop => uop.state.fpr(uop.rv.rs3))
        })
      case node: Term.Ascribe =>
        require(node.expr.toString().count(_ == '?') == 1, s"Unable to interpret ${node.expr}")
        val exprs = node.expr.toString().split('?')
        val (cond, lhs, rhs) = (apply(exprs(0)), apply(exprs(1)), apply(node.tpe.toString()))
        context.push(cond.mux(lhs, rhs))
    }
  }

  private val spikeOpCache = scala.collection.mutable.Map[String, Instruction](
    "CLZ" -> StateTransition.xprWrite(UValue(Spike.clz)),
    "CLZW" -> StateTransition.xprWrite(UValue(Spike.clzw)),
    "CTZ" -> StateTransition.xprWrite(UValue(Spike.ctz)),
    "CTZW" -> StateTransition.xprWrite(UValue(Spike.ctzw)),
    "CPOP" -> StateTransition.xprWrite(UValue(Spike.cpop)),
    "CPOPW" -> StateTransition.xprWrite(UValue(Spike.cpopw)),
    "ORC.B" -> StateTransition.xprWrite(UValue(Spike.orc_b)),
    "REV8" -> StateTransition.xprWrite(UValue(Spike.rev8)),
    "ZEXT.H" -> StateTransition.xprWrite(UValue(Spike.zexth)),
    "CLMUL" -> StateTransition.xprWrite(UValue(Spike.clmul)),
    "CLMULH" -> StateTransition.xprWrite(UValue(Spike.clmulh)),
    "CLMULR" -> StateTransition.xprWrite(UValue(Spike.clmulr)),
  )

  def fromSpike(instrName: String): Instruction = {
    try {
      spikeOpCache.getOrElseUpdate(
        instrName, {
          val context: Context = Block()
          Spike.getInstructionTemplate(instrName).foldLeft(context) { case (res, s) => apply(s)(res) }
        },
      )
    } catch {
      case e: Throwable =>
        println(s"Unable to interpret $instrName using the Spike instruction template:")
        println(Spike.getInstructionTemplate(instrName).mkString("\n"))
        throw e
    }
  }
}
