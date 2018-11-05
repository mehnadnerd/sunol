package sunol

import chisel3._

class RVInstruction extends Bundle {
  val full = UInt(32.W)
  val opcode = full(6, 0)
}

object RVInstruction {
  def apply(): RVInstruction = {
    new RVInstruction()
  }
}


object Util {

  implicit class ZextSext(u: UInt) {
    def sext: UInt = {
      u.asSInt().asTypeOf(SInt(32.W)).asUInt() //TODO: is this remotely correct
    }

    def zext: UInt = {
      u.asTypeOf(UInt(32.W))
    }
  }
}

object Constants {
  val OPCODE_OP_IMM = 0x13
  val OPCODE_OP = 0x33
  val OPCODE_AUIPC = 0x17
  val OPCODE_LUI = 0x37
  val OPCODE_LOAD = 0x03
  val OPCODE_STORE = 0x23
  val OPCODE_BRANCH = 0x63
  val OPCODE_JALR = 0x67
  val OPCODE_JAL = 0x6F
  val OPCODE_SYSTEM = 0x73
  val OPCODE_MISC_MEM = 0x0F
}
