import chisel3._
import chisel3.util._

package object svm {
  implicit class MacroStringConversion(s: String) {
    def macroToBigInt: BigInt = definitions.Spike.getEncoding(s.toUpperCase)

    def macroToInt: Int = s.macroToBigInt.toInt

    def macroToUInt: UInt = toUInt(s.macroToBigInt)

    def macroToUInt(width: Width): UInt = toUInt(s.macroToBigInt, width)

    def dotAsUnderscore: String = s.replace('.', '_')
  }

  implicit class MacroStringInterpolator(val sc: StringContext) extends AnyVal {
    def u(args: Any*): UInt = toUInt(big(args))

    def u64(args: Any*): UInt = toUInt(big(args), 64.W)

    def i(args: Any*): Int = big(args).toInt

    def big(args: Any*): BigInt = {
      require(sc.parts.length == 1, s"must have only one word instead of ${sc.parts}")
      val target = sc.parts.head
      require(!target.contains(" "), s"the parser target must be a word instead of \"$target\"")
      target.macroToBigInt
    }
  }

  def SignExt(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, a(aLen - 1)), a)
  }

  def ZeroExt(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, false.B), a)
  }

  def OneExt(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len - 1, 0) else Cat(Fill(len - aLen, true.B), a)
  }

  def firstTimeValid(valid: Bool): Bool = {
    val has_first = RegInit(false.B)
    val is_first = !has_first && valid
    when(is_first && !has_first) {
      has_first := true.B
    }
    is_first
  }

  def get_field(data: UInt, mask: BigInt): UInt = {
    val num_leading_zeros = java.lang.Long.numberOfLeadingZeros(mask.toLong)
    val high_index = java.lang.Long.numberOfLeadingZeros(0) - 1 - num_leading_zeros
    val num_trailing_zeros = log2Ceil(mask & ~(mask << 1))
    data(high_index, num_trailing_zeros)
  }
  def get_field(data: UInt, mask: String): UInt = get_field(data, mask.macroToBigInt)
  def get_bit(data: UInt, mask: String): Bool = {
    val field = get_field(data, mask.macroToBigInt)
    require(field.getWidth == 1, s"$mask is not one bit")
    field.asBool
  }

  def set_field(orig_data: UInt, mask: Int, update_data: UInt): UInt = set_field(orig_data, BigInt(mask), update_data)
  def set_field(orig_data: UInt, mask: BigInt, update_data: UInt): UInt = {
    set_field(orig_data, Seq(mask), Seq(update_data))
  }
  def set_field(orig_data: UInt, fields: Seq[(String, UInt)]): UInt = {
    set_field(orig_data, fields.map(_._1.macroToBigInt), fields.map(_._2))
  }
  def set_field(orig_data: UInt, mask: Seq[BigInt], update_data: Seq[UInt]): UInt = {
    val w = orig_data.getWidth
    val do_w = (x: Bits) => ZeroExt(x.asUInt, w)
    val full_mask = do_w(mask.reduce(_ | _).U)
    val shifted_data = update_data.zip(mask).map(x => do_w(do_w(x._1) << log2Ceil(x._2 & ~(x._2 << 1))))
    orig_data & (~full_mask).asUInt | shifted_data.reduce(_ | _)
  }

  private def toUInt(value: BigInt): UInt = s"h${value.toString(16)}".U

  private def toUInt(value: BigInt, width: Width): UInt = s"h${value.toString(16)}".U(width)
}
