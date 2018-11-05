package sunol

import chisel3._
import chisel3.util.Cat


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
    val stall = Input(Bool())
    val csr = Output(UInt(32.W))
  })
  val core = Module(new SunolCore())
  val dmem = core.io.dmem
  val imem = core.io.imem

  io.dcache_addr := dmem.addr
  io.icache_addr := imem.addr
  val sizemask = Mux(dmem.size === 0.U, 1.U(4.W), Mux(dmem.size === 1.U, 3.U(4.W), 7.U(4.W)))
  io.dcache_we := Cat(Seq.fill(4)(dmem.we)) & sizemask //TODO: fix this - should be
  io.dcache_re := dmem.re
  io.icache_re := imem.re
  io.dcache_din := dmem.wdata
  dmem.rdata := io.dcache_dout
  imem.data := io.icache_dout

  dmem.resp := !io.stall
  imem.resp := !io.stall
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