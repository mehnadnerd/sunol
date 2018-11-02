package sunol

import chisel3._

class SunolCore extends Module {
  override def io = IO(new Bundle {
    val imem = Flipped(new SunolIMemIO())
    val dmem = Flipped(new SunolDMemIO())
    val tohost = Output(UInt(32.W))
  })

  val pc = RegInit(0.U(32.W))

}

