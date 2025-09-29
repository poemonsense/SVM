package svm

import chisel3._
import chisel3.util._
import difftest.DifftestBundle
import difftest.bist.BISTParams
import difftest.util.DifftestProfile
import svm.definitions.RVInstructions

import scala.reflect.runtime.universe
import scala.reflect.runtime.currentMirror

case class SVMParams(
  bundleTypes: Seq[DifftestBundle],
  isa: String = "RV64IMAC",
  hartId: Int = 0,
  paddrBits: Int = 32,
  withCache: Option[Int] = None,
  cacheWays: Int = 16,
  cacheBanks: Int = 1,
  cacheReplacement: String = "none",
  cacheSRAMPorts: Int = 0,
  cacheRefillOnReadMiss: Boolean = false,
  withSRAM: (Long, Long) = (0x10000L, 0x10000L),
  peripheralBase: Long = 0x40000000L,
  withFlash: Option[(Long, Long)] = Some((0x10000000L, 0x10000000L)),
  memBase: Long = 0x80000000L,
  // This is the DUT memSize. REF should be on top of it. Default: 2GB.
  memSize: Long = 2048L * 1024 * 1024,
  memBeatSize: Int = 1, // number of words
  illegalInstrTval: Boolean = false,
  ebreakAsTrap: Boolean = false,
  haveFSDirty: Boolean = true,
  counterenMask: String = "h0",
  hasASID: Boolean = false,
  vendorid: Int = 0,
  archid: Int = 0,
  impid: Int = 0,
  enableDebug: Boolean = true,
  platform: String = "sim",
) {
  def isSim: Boolean = platform == "sim"
  def isFPGA: Boolean = platform == "fpga"
  def isPalladium: Boolean = platform == "palladium"

  require(isa.toUpperCase.startsWith("RV64"), s"ISA string is expected to start with RV64: $isa")
  private val extensions = isa.substring(4).toUpperCase.split('_')

  def all_insn: Seq[String] = parseIsaString.flatMap(RVInstructions.apply) ++ RVInstructions.baseISA
  def nInsn: Int = all_insn.length

  def nCommits: Int = bundleTypes.count(_.desiredCppName == "commit")

  def nInflight: Int = 1 << log2Ceil(nCommits * pipe_latency)
  def idWidth: Int = log2Ceil(nInflight)
  def idW: Width = idWidth.W

  def hasExtension(c: Char): Boolean = extensions.head.contains(c.toUpper)
  def hasExtension(s: String): Boolean = extensions.tail.contains(s.toUpperCase)

  def hasFsStatus: Boolean = hasExtension('D') || hasExtension('F') || haveFSDirty
  def hasCustomExtension: Boolean = false

  def hasVectoredTvec: Boolean = false
  def vmLevels: Int = 3

  def numCSRs: Map[String, Int] = Map("PMPCFG" -> 4, "PMPADDR" -> 4)

  def paddrWidth: Int = log2Ceil(memBase + memSize)

  def hasFlash: Boolean = withFlash.isDefined
  def isInFlash(addr: UInt): Bool = {
    withFlash.map { case (base, size) => addr >= base.U && addr < (base + size).U }.getOrElse(false.B)
  }
  def isInMem(addr: UInt): Bool = addr >= memBase.U && addr < (memBase + memSize).U
  def isInSRAM(addr: UInt): Bool = addr >= withSRAM._1.U && addr < (withSRAM._1 + withSRAM._2).U
  def isInPeripheral(addr: UInt): Bool = addr >= peripheralBase.U && addr < (peripheralBase + 0x8000).U

  def pipe_latency: Int = 24
  def store_load_gap: Int = 7
  def store_dmmu_gap: Int = 9

  // Stores are delayed to the last store stage to avoid WAR conflicts.
  def storeDelay(i: Int): Int = pipe_latency * (nCommits - 1 - i)

  // Load should wait for previous stores to take effect (expect the store).
  // 1) stores in previous commit slots: delayed until this load expectation
  // 2) stores in the same commit slot: until the store request stage
  def loadExpectDelay(i: Int): Int = storeDelay(i) + store_load_gap

  // DMMU also waits for previous stores to take effect (expect the store).
  def dmmuExpectDelay(i: Int): Int = storeDelay(i) + store_dmmu_gap

  private def parseIsaString: Seq[String] = {
    val xlen = isa.substring(2, 4).toInt
    extensions.head.toIndexedSeq.map(c => s"RV$xlen${c.toString}") ++ extensions.tail
  }

  def toBISTParams: BISTParams = BISTParams(bundleTypes, memBeatSize)
}

object SVMParams {
  def DefaultConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = SVMParams(bundleTypes = bundleTypes)

  def NutShellConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = DefaultConfig(bundleTypes).copy(
    counterenMask = "hffffffffffffffff",
    withFlash = None,
    memBeatSize = 1,
  )

  def PiperConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = DefaultConfig(bundleTypes).copy(
    isa = "RV64I_Xpiper",
    withFlash = Some(0x30000000L, 0x10000L),
    illegalInstrTval = true,
    ebreakAsTrap = true,
  )

  def RocketConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = DefaultConfig(bundleTypes).copy(
    isa = "RV64IMAFDC",
    illegalInstrTval = true,
    haveFSDirty = false,
  )

  def XiangShanConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = DefaultConfig(bundleTypes).copy(
    isa = "RV64IMAFDCH_Zba_Zbb_Zbc_Zbs", paddrBits = 36, archid = 25, hasASID = true, memBeatSize = 4,
  )

  def XiangShanMinimalConfig(bundleTypes: Seq[DifftestBundle]): SVMParams = DefaultConfig(bundleTypes).copy(
    isa = "RV64IMAFDCH_Zba_Zbb_Zbc_Zbs", paddrBits = 36, archid = 25, hasASID = true, memBeatSize = 4,
  )

  def fromString(config: String, bundleTypes: Seq[DifftestBundle]): SVMParams = {
    val methodSymbol = universe.typeOf[SVMParams.type].decl(universe.TermName(config)).asMethod
    val methodMirror = currentMirror.reflect(SVMParams).reflectMethod(methodSymbol)
    methodMirror(bundleTypes).asInstanceOf[SVMParams]
  }

  private val arg0 = Seq("--disable-debug")
  private val arg1 = Seq("--dut-profile", "--golden-config", "--platform", "--cache-size", "--cache-ways",
    "--cache-banks", "--cache-repl", "--cache-sram-ports", "--cache-refill-on-miss")
  private def readArgs(args: List[String]): (Map[String, String], List[String]) = {
    args match {
      case Nil => (Map(), List())
      case key :: tail =>
        val (value, next) = if (arg1.contains(key)) (tail.head, tail.tail) else ("", tail)
        val (argMap, others) = readArgs(next)
        if ((arg0 ++ arg1).contains(key)) {
          (argMap.updated(key, value), others)
        } else {
          (argMap, key +: others)
        }
    }
  }

  private def parseCacheSize(arg: String): Int = {
    val size = arg.toUpperCase
    if (size.endsWith("MB")) {
      size.replace("MB", "").toInt * 1024 * 1024
    } else if (size.endsWith("KB")) {
      size.replace("KB", "").toInt * 1024
    } else {
      size.toInt
    }
  }

  def fromArgs(args: Array[String]): (SVMParams, Array[String]) = {
    val (options, others) = readArgs(args.toList)
    val config = options.getOrElse("--golden-config", "DefaultConfig")
    val profileFile = options.get("--dut-profile")
    require(profileFile.isDefined, "you must specify the difftest profile")
    val bundleTypes = DifftestProfile.fromJson(profileFile.get).bundles.map(_.toBundle)
    val params = options.foldRight(fromString(config, bundleTypes)) { case ((key, value), p) =>
      key match {
        case "--disable-debug"    => p.copy(enableDebug = false)
        case "--platform"         => p.copy(platform = value)
        case "--cache-size"       => p.copy(withCache = Some(parseCacheSize(value)))
        case "--cache-ways"       => p.copy(cacheWays = value.toInt)
        case "--cache-banks"      => p.copy(cacheBanks = value.toInt)
        case "--cache-repl"       => p.copy(cacheReplacement = value)
        case "--cache-sram-ports" => p.copy(cacheSRAMPorts = value.toInt)
        case "--cache-refill-on-miss" =>
          val ports = value.split(",")
          ports.foreach(port => require(Seq("read", "write").contains(port), s"$port not allowed"))
          p.copy(cacheRefillOnReadMiss = ports.contains("read"))
        case _ => p
      }
    }
    (params, others.toArray)
  }
}
