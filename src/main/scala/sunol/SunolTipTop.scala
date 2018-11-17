package sunol

import chisel3._

class SunolTipTop extends Module {
  val io = IO(new Bundle {
    val mem_req_valid = Output(Bool())
    val mem_req_ready = Input(Bool())
    val mem_req_rw = Output(Bool())
    val mem_req_addr = Output(UInt(28.W))
    val mem_req_tag = Output(UInt(5.W))

    val mem_req_data_valid = Output(Bool())
    val mem_req_data_ready = Input(Bool())
    val mem_req_data_bits = Output(UInt(128.W))
    val mem_req_data_mask = Output(UInt(16.W))

    val mem_resp_valid = Input(Bool())
    val mem_resp_tag = Input(UInt(5.W))
    val mem_resp_data = Input(UInt(128.W))
    val csr = Output(UInt(32.W))
  })

  val core = Module(new SunolCore)
  io.csr := core.io.tohost

  val mem = Module(new SunolMem)
  core.io.dmem <> mem.io.dmem
  core.io.imem <> mem.io.imem {
    val mm = mem.io.mainmem

    io.mem_req_valid := mm.req_valid
    mm.req_ready := io.mem_req_ready
    io.mem_req_rw := mm.req_rw
    io.mem_req_addr := mm.req_addr
    io.mem_req_tag := mm.req_tag

    io.mem_req_data_valid := mm.req_data_valid
    mm.req_data_ready := io.mem_req_data_ready
    io.mem_req_data_bits := mm.req_data_bits
    io.mem_req_data_mask := mm.req_data_mask

    mm.resp_valid := io.mem_resp_valid
    mm.resp_tag := io.mem_resp_tag
    mm.resp_data := io.mem_resp_data
  }

}

//module riscv_top
//(
//input clk,
//input reset,
//
//output                      mem_req_valid,
//input                       mem_req_ready,
//output                      mem_req_rw,
//output [`MEM_ADDR_BITS-1:0] mem_req_addr,
//output [`MEM_TAG_BITS-1:0]  mem_req_tag,
//
//output                      mem_req_data_valid,
//input                       mem_req_data_ready,
//output [`MEM_DATA_BITS-1:0] mem_req_data_bits,
//output [(`MEM_DATA_BITS/8)-1:0] mem_req_data_mask,
//
//input                       mem_resp_valid,
//input [`MEM_TAG_BITS-1:0]   mem_resp_tag,
//input [`MEM_DATA_BITS-1:0]  mem_resp_data,
//output [31:0]               csr
//);