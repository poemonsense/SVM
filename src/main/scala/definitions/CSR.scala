package svm.definitions

import chisel3._
import chisel3.util._
import svm._

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

trait HasCSRConstants {
  def legalize_privilege(prv: UInt): UInt = Mux(prv === u"PRV_HS", u"PRV_U", prv)
}

// This class should be inherited for definitions of CSRs whose class names should end with CSRDef.
abstract class CSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends HasCSRConstants {
  private val rhs = curr.getOrElse(RegInit(init))
  private val lhs = next.getOrElse(rhs)

  def name = getClass.getName.split('.').last.toUpperCase.replace("CSRDEF", "")
  def is(thatCsrName: String) = name == thatCsrName
  def address: Int = s"CSR_$name".macroToInt
  protected def init: UInt = 0.U(64.W)

  def read: UInt = rhs

  def write(wdata: UInt): Unit = lhs := wdata
  def set(mask: UInt): Unit = write(read | ZeroExt(mask, 64))
  def clear(mask: UInt): Unit = write(read & ZeroExt((~mask).asUInt, 64))

  def writeSideEffect(csrs: Seq[CSRDef]): Unit = {}

  def exist: Boolean = true
}

abstract class ProxyCSRDef(delegate: CSRDef)(implicit p: SVMParams) extends CSRDef(Some(delegate.read), None) {
  val proxy_mask: UInt

  override def read: UInt = delegate.read & proxy_mask
  override def write(wdata: UInt): Unit = delegate.write(wdata & proxy_mask | delegate.read & (~proxy_mask).asUInt)
}

abstract class BaseStatusCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends CSRDef(curr, next) {
  def sstatus_write_mask: UInt = {
    val fs = if (p.hasFsStatus) u"SSTATUS_FS" else 0.U
    u64"SSTATUS_SIE" | u"SSTATUS_SPIE" | u"SSTATUS_SPP" | u"SSTATUS_SUM" | u"SSTATUS_MXR" | fs
  }

  def adjust_sd(data: UInt): UInt = {
    val sd_fields = Option.when(p.hasFsStatus)(u"SSTATUS_FS") ++
      Option.when(p.hasExtension('V'))(u"SSTATUS_VS") ++
      Option.when(p.hasCustomExtension)(u"SSTATUS_XS")
    if (sd_fields.nonEmpty) {
      val is_set = VecInit(sd_fields.map(m => (data & m) === m).toSeq).asUInt.orR
      val sd_bit = u64"SSTATUS64_SD"
      Mux(is_set, data | sd_bit, data & (~sd_bit).asUInt)
    } else {
      data
    }
  }

  def adjust_fs(data: UInt): UInt = {
    if (p.haveFSDirty) data else set_field(data, i"MSTATUS_FS", Fill(2, get_field(data, i"MSTATUS_FS").orR))
  }
}

class MstatusCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends BaseStatusCSRDef(curr, next) {
  override def write(wdata: UInt): Unit = {
    val mstatus_mask = u64"MSTATUS_MIE" | u"MSTATUS_MPIE" | u"MSTATUS_MPRV" |
      u"MSTATUS_MPP" | u"MSTATUS_TW" | u"MSTATUS_TSR" | u"MSTATUS_TVM"
    val mask = sstatus_write_mask | mstatus_mask
    val requested_mpp = legalize_privilege(get_field(wdata, i"MSTATUS_MPP"))
    val adjusted_val = set_field(wdata, i"MSTATUS_MPP", requested_mpp)
    val new_mstatus = (read & (~mask).asUInt) | (adjusted_val & mask)
    super.write(adjust_sd(adjust_fs(new_mstatus)))
  }
}

class SstatusCSRDef(mstatus: MstatusCSRDef)(implicit p: SVMParams) extends ProxyCSRDef(mstatus) {
  override val proxy_mask: UInt = mstatus.sstatus_write_mask | u"SSTATUS_UBE" | u"SSTATUS_UXL" | u"SSTATUS64_SD"
}

abstract class EpcCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next) {
  override def write(wdata: UInt): Unit = {
    val mask = if (p.hasExtension('C')) 1 else 3
    super.write(wdata & (~mask.U(64.W)).asUInt)
  }
}

class MepcCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends EpcCSRDef(curr, next)

class SepcCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends EpcCSRDef(curr, next)

class MtvalCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class Mtval2CSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class MtinstCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class MconfigptrCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class MenvcfgCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class MsecCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class StvalCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

abstract class XtvecCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next) {
  override def write(wdata: UInt): Unit = {
    val mask = if (p.hasVectoredTvec) 2 else 3
    super.write(wdata & (~mask.U(64.W)).asUInt)
  }
}

class MtvecCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends XtvecCSRDef(curr, next)

class StvecCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends XtvecCSRDef(curr, next)

class McauseCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class ScauseCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class SatpCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next) {
  override def write(wdata: UInt): Unit = {
    val rv64_ppn_mask = (1L << (p.paddrBits - 12)) - 1
    val mode_mask = u64"SATP64_MODE"
    val asid_mask = if (p.hasASID) u64"SATP64_ASID" else 0.U(64.W)
    val ppn_mask = u64"SATP64_PPN" & rv64_ppn_mask.U(64.W)
    val mask = mode_mask | asid_mask | ppn_mask
    when(satp_valid(wdata)) {
      super.write(mask & wdata)
    }
  }

  private def satp_valid(data: UInt): Bool = {
    val mode = get_field(data, big"SATP64_MODE")
    mode === u"SATP_MODE_OFF" || mode === u"SATP_MODE_SV39"
  }
}

abstract class MaskedWriteCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends CSRDef(curr, next) {
  def get_mask: UInt
  override def write(wdata: UInt): Unit = {
    val mask = get_mask
    super.write(read & (~mask).asUInt | (wdata & mask))
  }
}

class MipCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends MaskedWriteCSRDef(curr, next) {
  override def get_mask: UInt = {
    val supervisor_ints = u64"MIP_SSIP" | u"MIP_STIP" | u"MIP_SEIP"
    supervisor_ints & (u"MIP_SEIP" | u"MIP_SSIP" | u"MIP_STIP" | u"MIP_LCOFIP")
  }
}

class MieCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends MaskedWriteCSRDef(curr, next) {
  override def get_mask: UInt = {
    val supervisor_ints = u64"MIP_SSIP" | u"MIP_STIP" | u"MIP_SEIP"
    supervisor_ints | u"MIP_MSIP" | u"MIP_MTIP" | u"MIP_MEIP"
  }
}

class MscratchCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class SscratchCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next)

class MidelegCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, next) {
  private val hypervisor_ints = if (false /*p.hasExtension('H')*/ ) u64"MIDELEG_FORCED_MASK" else 0.U(64.W)

  override def write(wdata: UInt): Unit = {
    val supervisor_ints = u64"MIP_SSIP" | u"MIP_STIP" | u"MIP_SEIP"
    super.write((wdata & supervisor_ints) | hypervisor_ints)
  }

  override def read: UInt = super.read | hypervisor_ints
}

class MedelegCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends MaskedWriteCSRDef(curr, next) {
  override def get_mask: UInt = {
    val hypervisor_exceptions = if (false /*p.hasExtension('H')*/ ) {
      (1 << i"CAUSE_VIRTUAL_SUPERVISOR_ECALL") |
        (1 << i"CAUSE_FETCH_GUEST_PAGE_FAULT") |
        (1 << i"CAUSE_LOAD_GUEST_PAGE_FAULT") |
        (1 << i"CAUSE_VIRTUAL_INSTRUCTION") |
        (1 << i"CAUSE_STORE_GUEST_PAGE_FAULT")
    } else 0
    ((1 << i"CAUSE_MISALIGNED_FETCH") |
      (1 << i"CAUSE_FETCH_ACCESS") |
      (1 << i"CAUSE_ILLEGAL_INSTRUCTION") |
      (1 << i"CAUSE_BREAKPOINT") |
      (1 << i"CAUSE_MISALIGNED_LOAD") |
      (1 << i"CAUSE_LOAD_ACCESS") |
      (1 << i"CAUSE_MISALIGNED_STORE") |
      (1 << i"CAUSE_STORE_ACCESS") |
      (1 << i"CAUSE_USER_ECALL") |
      (1 << i"CAUSE_SUPERVISOR_ECALL") |
      (1 << i"CAUSE_FETCH_PAGE_FAULT") |
      (1 << i"CAUSE_LOAD_PAGE_FAULT") |
      (1 << i"CAUSE_STORE_PAGE_FAULT") |
      hypervisor_exceptions).U(64.W)
  }
}

abstract class UnwritableCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends CSRDef(curr, None) {
  override def write(wdata: UInt): Unit = {}
}

class MisaCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def init: UInt = {
    // RV64I with Supervisor and User modes
    val base = "h8000000000140100".U(64.W)
    val exts = Seq('A', 'C', 'D', 'F', 'H', 'M', 'Q', 'V')
    val ext = exts.flatMap(c => Option.when(p.hasExtension(c))(1 << (c - 'A'))).reduce(_ | _).U(64.W)
    base | ext
  }
}

class MvendoridCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def init: UInt = p.vendorid.U
}

class MarchidCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def init: UInt = p.archid.U
}

class MimpidCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def init: UInt = p.impid.U
}

class PmpaddrCSRDef(i: Int, curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def name: String = s"PMPADDR$i"

  override def is(thatCsrName: String): Boolean = thatCsrName.filterNot(_.isDigit) == name.filterNot(_.isDigit)

  // TODO: this init value is a temp patch for XS. Need fix it.
  override def init: UInt = {
    if (p.hasExtension("Zba") && i > 0) {
      ZeroExt(Cat(Fill(p.paddrBits - 12, true.B), 0.U(10.W)), 64)
    } else {
      ZeroExt(Fill(p.paddrBits - 2, true.B), 64)
    }
  }
}

class PmpcfgCSRDef(i: Int, curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def name: String = s"PMPCFG$i"
  override def is(thatCsrName: String): Boolean = thatCsrName.filterNot(_.isDigit) == name.filterNot(_.isDigit)
}

class MhartidCSRDef(curr: Option[UInt])(implicit p: SVMParams) extends UnwritableCSRDef(curr) {
  override def init: UInt = p.hartId.U(64.W)
}

class McounterenCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends MaskedWriteCSRDef(curr, next) {
  override def get_mask: UInt = p.counterenMask.U(64.W)
}

class ScounterenCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams)
  extends McounterenCSRDef(curr, next)

class SieCSRDef(mie: MieCSRDef)(implicit p: SVMParams) extends ProxyCSRDef(mie) {
  override val proxy_mask: UInt = u"MIP_S_MASK"
}

class SipCSRDef(mip: MipCSRDef)(implicit p: SVMParams) extends ProxyCSRDef(mip) {
  override val proxy_mask: UInt = u"MIP_S_MASK"
}

class FcsrCSRDef(curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams) extends MaskedWriteCSRDef(curr, next) {
  override def get_mask: UInt = 0xff.U

  override def writeSideEffect(csrs: Seq[CSRDef]): Unit = {
    val mstatus = csrs.find(_.name == "MSTATUS").get
    mstatus.set(u64"MSTATUS64_SD" | u64"MSTATUS_FS")
  }

  override def exist: Boolean = p.hasExtension('D') || p.hasExtension('F')
}

class FflagsCSRDef(fcsr: FcsrCSRDef)(implicit p: SVMParams) extends CSRDef(None, None) {
  override def read: UInt = fcsr.read(4, 0)
  override def write(wdata: UInt): Unit = fcsr.write(Cat(fcsr.read(7, 5), wdata(4, 0)))
  override def writeSideEffect(csrs: Seq[CSRDef]): Unit = fcsr.writeSideEffect(csrs)
  override def exist: Boolean = p.hasExtension('D') || p.hasExtension('F')
}

class FrmCSRDef(fcsr: FcsrCSRDef)(implicit p: SVMParams) extends CSRDef(None, None) {
  override def read: UInt = fcsr.read(7, 5)
  override def write(wdata: UInt): Unit = fcsr.write(Cat(wdata(2, 0), fcsr.read(4, 0)))
  override def writeSideEffect(csrs: Seq[CSRDef]): Unit = fcsr.writeSideEffect(csrs)
  override def exist: Boolean = p.hasExtension('D') || p.hasExtension('F')
}

object CSRDef {
  private val currentPackage = getClass.getPackage.getName
  private def loadCSRClassSymbol(csrName: String, force: Boolean = false)(implicit
    p: SVMParams
  ): Option[ClassSymbol] = {
    val className = toClassName(csrName)
    try {
      Some(currentMirror.staticClass(className))
    } catch {
      case e: ScalaReflectionException =>
        require(!force, s"CSR definition not found: $csrName => $className\n$e")
        None
    }
  }
  private def loadConstructor(classSymbol: ClassSymbol): (MethodSymbol, MethodMirror) = {
    val classMirror = currentMirror.reflectClass(classSymbol)
    val constructorSymbol = classSymbol.primaryConstructor.asMethod
    val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
    (constructorSymbol, constructorMirror)
  }

  // Convert a csrName to className
  def toClassName(csrName: String)(implicit p: SVMParams) = {
    // CSRs like PMPCFG0, PMPCFG1, ... are converted into PMPCFG.
    val hasIndices = p.numCSRs.contains(csrName.filterNot(_.isDigit).toUpperCase)
    val defName = if (hasIndices) csrName.filterNot(_.isDigit) else csrName
    s"$currentPackage.${defName(0).toUpper}${defName.toLowerCase.substring(1)}CSRDef"
  }

  // Convert a className to csrName
  def toCSRName(className: String) = className.split('.').last.toUpperCase.replace("CSRDEF", "")

  // Names for all CSRs defined in the current package.
  def csrNames()(implicit p: SVMParams): Seq[String] = {
    val symbols = Spike.csrNames.flatMap(name => loadCSRClassSymbol(name))
    symbols.map(symbol => toCSRName(symbol.fullName)).distinct
  }

  // This an intelligent method for creating CSR definitions with autofill features for indices and delegate.
  // We record every CSR definition here to allow automatic delegation using the definition history.
  // This autofill mechanism may not work as expected when the last matching CSR is not expected to be the delegate.
  // Therefore, be careful when using this method and make sure you know what these comments mean.
  private val definitions = scala.collection.mutable.Map.empty[String, CSRDef]
  def apply(csrName: String, curr: Option[UInt], next: Option[UInt])(implicit p: SVMParams): Seq[CSRDef] = {
    val classSymbol = loadCSRClassSymbol(csrName, force = true).get
    val (constructorSymbol, constructorMirror) = loadConstructor(classSymbol)
    val argumentNames = constructorSymbol.paramLists.flatten.map(_.name.toString)
    val csrNameHasDigit = csrName.exists(_.isDigit)
    val numDefs = if (csrNameHasDigit) 1 else p.numCSRs.getOrElse(csrName, 1)
    val csrDefs = Seq.tabulate(numDefs)(i => {
      val arguments = argumentNames.map {
        case "curr" => curr
        case "next" => next
        case "i"    => if (csrNameHasDigit) csrName.filter(_.isDigit).toInt else i
        case "p"    => p
        // Other unknown arguments will all be viewed as delegates.
        case delegate => definitions.get(delegate.toLowerCase).orNull
      }
      val csrDef = constructorMirror.apply(arguments: _*).asInstanceOf[CSRDef]
      Option.when(csrDef.exist) {
        require(!arguments.contains(null), "some arguments are not found")
        definitions.update(csrDef.name.toLowerCase, csrDef)
        csrDef
      }
    })
    csrDefs.flatten
  }

  // Create the CSR definition class with both input and output values
  def apply(csrName: String, curr: UInt, next: UInt)(implicit p: SVMParams): Seq[CSRDef] = {
    apply(csrName, Some(curr), Some(next))
  }

  // Create the CSR definition class without input/output values
  def apply(csrName: String)(implicit p: SVMParams): Seq[CSRDef] = {
    apply(csrName, None, None)
  }
}
