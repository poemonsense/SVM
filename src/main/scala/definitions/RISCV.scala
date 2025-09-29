package svm.definitions

import chisel3._
import chisel3.util._
import difftest.InstrCommit

object RVInstructions {
  val RV32I = Seq(
    "LUI", "AUIPC", "JAL", "JALR", "BEQ", "BNE", "BLT", "BGE", "BLTU", "BGEU", "LB", "LH", "LW", "LBU", "LHU", "SB",
    "SH", "SW", "ADDI", "SLTI", "SLTIU", "XORI", "ORI", "ANDI", "SLLI", "SRLI", "SRAI", "ADD", "SUB", "SLL", "SLT",
    "SLTU", "XOR", "SRL", "SRA", "OR", "AND", "FENCE", "PAUSE", "ECALL", "EBREAK",
  )
  val RV64I = RV32I ++ Seq(
    "LWU", "LD", "SD", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW",
  )
  val Zifencei = Seq("FENCE.I")
  val Zicsr = Seq("CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI")
  val RV32M = Seq("MUL", "MULH", "MULHSU", "MULHU", "DIV", "DIVU", "REM", "REMU")
  val RV64M = RV32M ++ Seq("MULW", "DIVW", "DIVUW", "REMW", "REMUW")
  val Privileged = Seq("SRET", "MRET", "WFI", "SFENCE.VMA")
  val RV64A = Seq(
    "LR.W", "SC.W", "AMOSWAP.W", "AMOADD.W", "AMOXOR.W", "AMOAND.W", "AMOOR.W", "AMOMIN.W", "AMOMAX.W", "AMOMINU.W",
    "AMOMAXU.W", "LR.D", "SC.D", "AMOSWAP.D", "AMOADD.D", "AMOXOR.D", "AMOAND.D", "AMOOR.D", "AMOMIN.D", "AMOMAX.D",
    "AMOMINU.D", "AMOMAXU.D",
  )
  val RV32F = Seq(
    "FLW", "FSW", "FMADD.S", "FMSUB.S", "FNMSUB.S", "FNMADD.S", "FADD.S", "FSUB.S", "FMUL.S", "FDIV.S", "FSQRT.S",
    "FSGNJ.S", "FSGNJN.S", "FSGNJX.S", "FMIN.S", "FMAX.S", "FCVT.W.S", "FCVT.WU.S", "FMV.X.W", "FEQ.S", "FLT.S",
    "FLE.S", "FCLASS.S", "FCVT.S.W", "FCVT.S.WU", "FMV.W.X",
  )
  val RV64F = RV32F ++ Seq("FCVT.L.S", "FCVT.LU.S", "FCVT.S.L", "FCVT.S.LU")
  val RV32D = Seq(
    "FLD", "FSD", "FMADD.D", "FMSUB.D", "FNMSUB.D", "FNMADD.D", "FADD.D", "FSUB.D", "FMUL.D", "FDIV.D", "FSQRT.D",
    "FSGNJ.D", "FSGNJN.D", "FSGNJX.D", "FMIN.D", "FMAX.D", "FCVT.S.D", "FCVT.D.S", "FEQ.D", "FLT.D", "FLE.D",
    "FCLASS.D", "FCVT.W.D", "FCVT.WU.D", "FCVT.D.W", "FCVT.D.WU",
  )
  val RV64D = RV32D ++ Seq("FCVT.L.D", "FCVT.LU.D", "FMV.X.D", "FCVT.D.L", "FCVT.D.LU", "FMV.D.X")
  val RV64H = Seq("HFENCE.VVMA", "HFENCE.GVMA")
  val ZBA = Seq("ADD.UW", "SH1ADD", "SH1ADD.UW", "SH2ADD", "SH2ADD.UW", "SH3ADD", "SH3ADD.UW", "SLLI.UW")
  val ZBB = Seq("ANDN", "CLZ", "CLZW", "CPOP", "CPOPW", "CTZ", "CTZW", "MAX", "MAXU", "MIN", "MINU", "ORC.B", "ORN",
    "REV8", "ROL", "ROLW", "ROR", "RORI", "RORIW", "RORW", "SEXT.B", "SEXT.H", "XNOR", "ZEXT.H")
  val ZBC = Seq("CLMUL", "CLMULH", "CLMULR")
  val ZBS = Seq("BCLR", "BCLRI", "BEXT", "BEXTI", "BINV", "BINVI", "BSET", "BSETI")

  val Xpiper = Seq("MULW")

  def baseISA: Seq[String] = Zifencei ++ Zicsr ++ Privileged

  def system: Seq[String] = Seq("FENCE", "PAUSE", "ECALL", "EBREAK") ++ RVInstructions.Zifencei ++
    RVInstructions.Zicsr ++ RVInstructions.Privileged ++ RV64H

  def apply(extension: String): Seq[String] = {
    try {
      getClass.getDeclaredField(extension).get(this).asInstanceOf[Seq[String]]
    } catch {
      case _: NoSuchFieldException =>
        if (extension != "RV64C") {
          println(s"$extension is not supported and thus skipped. Please check your ISA string.")
        }
        Seq.empty
    }
  }
}

private object SignExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, a(aLen - 1)), a)
  }
}

private object MaskExpand {
  def apply(m: UInt, maskWidth: Int = 8): UInt = Cat(m.asBools.map(Fill(maskWidth, _)).reverse)
  def apply(m: Seq[Bool], maskWidth: Int): Vec[UInt] = VecInit(m.map(Fill(maskWidth, _)))
}

class RVInstruction(instr: UInt) {
  def get: UInt = instr

  def rs1: UInt = instr(19, 15)
  def rs2: UInt = instr(24, 20)
  def rs3: UInt = instr(27, 31)
  def rd: UInt = instr(11, 7)

  def sext_xlen(data: UInt): UInt = SignExt(data, 64)
  def i_imm(): UInt = sext_xlen(instr(31, 20))
  def s_imm(): UInt = sext_xlen(Cat(instr(31, 25), instr(11, 7)))
  def sb_imm(): UInt = sext_xlen(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)))
  def u_imm(): UInt = sext_xlen((instr(31, 12) << 12).asUInt)
  def uj_imm(): UInt = sext_xlen(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)))

  def csr: UInt = instr(31, 20)
  def csr_r: Bool = rd =/= 0.U // for CSRRW and CSRRWI
  def csr_w: Bool = rs1 =/= 0.U // for CSRRS/CSRRC/CSRRSI/CSRRCI

  def length: UInt = Mux(instr(1, 0) === "h3".U, 4.U, 2.U)

  def ===(that: UInt): Bool = instr === that
  def ===(that: BitPat): Bool = instr === that
  def =/=(that: UInt): Bool = instr =/= that

  def isLoad: Bool = instr(6, 0) === 0x3.U && instr(14, 12) =/= "b111".U
  def getLoadSize: UInt = (1.U << instr(13, 12)).asUInt
  def getLoadMask: UInt = MaskExpand(((1.U << getLoadSize).asUInt - 1.U)(7, 0))

  def toPrintable: Printable = p"${Hexadecimal(instr)}"
}

class RVMicroOp extends Bundle {
  val pc = UInt(64.W)
  val inst_val = UInt(32.W)
  val instr = new RVInstruction(inst_val)

  val is_interrupt = UInt(4.W)
  val is_instr_page_fault = Bool()
  val is_load_page_fault = Bool()
  val is_store_page_fault = Bool()
  val is_skip = Bool()
  val is_trap = Bool()
  val is_rvc = Bool()

  val rfwen = Bool()
  val fpwen = Bool()
  val rd_d = UInt(64.W)
  val div_data = UInt(64.W)
  val data_paddr = Valid(UInt(64.W))

  override def toPrintable: Printable = {
    val pc_p = p"pc ${Hexadecimal(pc)}"
    val inst_p = p"inst ${Hexadecimal(inst_val)}"
    val rf_p = p"wen ${rfwen || fpwen} dst ${instr.rd} data ${Hexadecimal(rd_d)}"
    val skip_p = p"S($is_skip)"
    Seq(pc_p, inst_p, rf_p, skip_p).reduce(_ + ", " + _)
  }

  def fromInstrCommit(commit: InstrCommit): RVMicroOp = {
    pc := commit.pc
    inst_val := commit.instr
    is_rvc := commit.isRVC
    is_interrupt := DontCare
    is_instr_page_fault := DontCare
    is_load_page_fault := DontCare
    is_store_page_fault := DontCare
    is_skip := commit.skip
    is_trap := DontCare
    rfwen := commit.rfwen
    fpwen := commit.fpwen
    rd_d := DontCare
    div_data := DontCare
    data_paddr := DontCare
    this
  }
}
