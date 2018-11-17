package sunol

import chisel3._
import chisel3.util.{Cat, Fill, MuxLookup, RegEnable}


class SunolTop extends Module {
  val io = IO(new Bundle {
    val dcache_addr = Output(UInt(32.W))
    val icache_addr = Output(UInt(32.W))
    val dcache_we = Output(UInt(4.W))
    val dcache_re = Output(Bool())
    val icache_re = Output(Bool())
    val dcache_din = Output(UInt(32.W))
    val dcache_dout = Input(UInt(32.W))
    val icache_dout = Input(UInt(32.W))
    val icache_resp_addr = Input(UInt(30.W))
    val stall = Input(Bool())
    val csr = Output(UInt(32.W))
  })
  val core = Module(new SunolCore())
  val dmem = core.io.dmem
  val imem = core.io.imem

  io.dcache_addr := dmem.addr
  io.icache_addr := imem.addr
  io.dcache_re := dmem.re
  io.icache_re := imem.re
  imem.data := io.icache_dout
  imem.resp_addr := io.icache_resp_addr

  imem.resp_addr := RegEnable(imem.addr, imem.re)
  dmem.resp_addr := RegEnable(dmem.addr, dmem.re || dmem.we) //TODO: is this correct

  // Handle writes
  val shift = WireInit(io.dcache_addr(1, 0))
  val sizemask = MuxLookup(dmem.size, 15.U(4.W), Array(0.U -> 1.U(4.W), 1.U -> 3.U(4.W))) << shift
  io.dcache_din := dmem.wdata << (shift << 3.U).asUInt()
  io.dcache_we := Cat(Seq.fill(4)(dmem.we)) & sizemask.asUInt()

  // Handle loads
  val sext = !dmem.size(2)

  dmem.rdata := io.dcache_dout
  when(dmem.size(1, 0) === 0.U) {
    dmem.rdata := Cat(Fill(24, sext & io.dcache_dout(7)), io.dcache_dout(7, 0))
    when(io.dcache_addr(1, 0) === 1.U) {
      dmem.rdata := Cat(Fill(24, sext & io.dcache_dout(15)), io.dcache_dout(15, 8))
    }.elsewhen(io.dcache_addr(1, 0) === 2.U) {
      dmem.rdata := Cat(Fill(24, sext & io.dcache_dout(23)), io.dcache_dout(23, 16))
    }.elsewhen(io.dcache_addr(1, 0) === 3.U) {
      dmem.rdata := Cat(Fill(24, sext & io.dcache_dout(31)), io.dcache_dout(31, 24))
    }
  }.elsewhen(dmem.size(1, 0) === 1.U) {
    dmem.rdata := Cat(Fill(16, sext & io.dcache_dout(15)), io.dcache_dout(15, 0))
    when(io.dcache_addr(1, 0) === 2.U) {
      dmem.rdata := Cat(Fill(16, sext & io.dcache_dout(31)), io.dcache_dout(31, 16))
    }
  }

  dmem.valid := !io.stall
  imem.valid := !io.stall
  //io.stall //TODO what is this
  io.csr := core.io.tohost
}


//verilog interface needed
//module Riscv141(
//input clk,
//input reset,
//
//// Memory system ports
//output [31:0] dcache_addr,
//output [31:0] icache_addr,
//output [3:0] dcache_we,
//output dcache_re,
//output icache_re,
//output [31:0] dcache_din,
//input [31:0] dcache_dout,
//input [31:0] icache_dout,
//input stall,
//output [31:0] csr
//
//);