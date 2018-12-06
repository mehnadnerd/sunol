package sunol

import chisel3._
import chisel3.util.{Cat, Fill}

class RVInstruction extends Bundle {
  val full = UInt(32.W)
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

class BranchPredictor extends Module {
  import Constants._

  val io = IO(new Bundle {
    val pc = Input(UInt(32.W))
    val inst = Input(RVInstruction())
    val taken = Output(Bool())
    val target = Output(UInt(32.W))
  })

  val opcode = io.inst.full(6, 0)
  val imm_b = Cat(Fill(20, io.inst.full(31)), io.inst.full(7), io.inst.full(30, 25), io.inst.full(11, 8), 0.U(1.W))
  val imm_j = Cat(Fill(12, io.inst.full(31)), io.inst.full(19, 12), io.inst.full(20), io.inst.full(30, 21), 0.U(1.W))

  val is_j = opcode === OPCODE_JAL.U
  val is_b = opcode === OPCODE_BRANCH.U

  // val offset = Mux(is_b, imm_b, imm_j)
  val offset = imm_b
  val sign = offset(31)

  // io.taken := is_j || (is_b && sign)
  io.taken := is_b
  io.target := Mux(io.taken, io.pc + offset, io.pc + 4.U)
}
