package sunol

import chisel3._

class SunolIMemIO extends Bundle {
  val addr = Input(UInt(32.W))
  val re = Input(Bool())
  val data = Output(UInt(32.W))
  val resp = Output(Bool())
}

class SunolDMemIO extends Bundle {
  val addr = Input(UInt(32.W))
  val size = Input(UInt(3.W))
  val re = Input(Bool())
  val wdata = Input(UInt(32.W))
  val we = Input(Bool())
  val rdata = Output(UInt(32.W))
  val resp = Output(Bool())
}

abstract class SunolMem extends Module {

}