package sunol

import chisel3._

// imem to core
class SunolIMemCoreIO extends Bundle {
  val addr = Input(UInt(32.W))
  val re = Input(Bool())

  val data = Output(UInt(32.W))
  val valid = Output(Bool())
  val resp_addr = Output(UInt(32.W))
}

// dmem to core
class SunolDMemCoreIO extends Bundle {
  val addr = Input(UInt(32.W))
  val size = Input(UInt(3.W))
  val re = Input(Bool())
  val wdata = Input(UInt(32.W))
  val we = Input(Bool())

  val rdata = Output(UInt(32.W))
  val valid = Output(Bool())
  val resp_addr = Output(UInt(32.W))
}

//`define MEM_DATA_BITS 128
//`define MEM_TAG_BITS 5
//`define MEM_ADDR_BITS 28
//`define MEM_DATA_CYCLES 4


// main mem to mem (Arbitrated to caches)
class SunolMainMemIO extends Bundle {
  val req_valid = Output(Bool())
  val req_ready = Input(Bool())
  val req_rw = Output(Bool())
  val req_addr = Output(UInt(28.W))
  val req_tag = Output(UInt(5.W))

  val req_data_valid = Output(Bool())
  val req_data_ready = Input(Bool())
  val req_data_bits = Output(UInt(128.W))
  val req_data_mask = Output(UInt(16.W))

  val resp_valid = Input(Bool())
  val resp_data = Input(UInt(128.W))
  val resp_tag = Input(UInt(5.W))
}

// caches to mem (arbiter out)
class SunolMainMemCacheIO extends Bundle {
  val read_req_valid = Output(Bool())
  val read_req_ready = Input(Bool())
  val read_req_rw = Output(Bool())
  val read_req_addr = Output(UInt(28.W))

  val write_req_data_valid = Output(Bool())
  val write_req_data_bits = Output(UInt(128.W))
  val write_req_data_mask = Output(UInt(16.W))
  val write_req_data_ready = Input(Bool())

  val read_resp_valid = Input(Bool())
  val read_resp_data = Input(UInt(128.W))
}

class SunolICache extends Module {
  val io = IO(new Bundle {
    val imem = new SunolIMemCoreIO
    val mem = new SunolMainMemCacheIO
  })
}

class SunolDCache extends Module {
  val io = IO(new Bundle {
    val dmem = new SunolDMemCoreIO
    val mem = new SunolMainMemCacheIO
  })
}

class SunolMem extends Module {
  val io = IO(new Bundle {
    val imem = new SunolIMemCoreIO
    val dmem = new SunolDMemCoreIO

    val mainmem = new SunolMainMemIO
  })

  val ic = Module(new SunolICache)
  val dc = Module(new SunolDCache)
  io.imem <> ic.io.imem
  io.dmem <> dc.io.dmem
  val mm = io.mainmem
  val icm = ic.io.mem
  val dcm = dc.io.mem

  //common
  for (cm <- Seq(ic.io.mem, dc.io.mem)) {
    cm.read_resp_data := mm.resp_data
  }

  //write (dcache only)
  mm.req_data_bits := dcm.write_req_data_bits
  mm.req_data_mask := dcm.write_req_data_mask
  mm.req_data_valid := dcm.write_req_data_valid
  dcm.write_req_data_ready := mm.req_data_ready

  icm.write_req_data_ready := false.B

  //arbiter
  {
    icm.read_req_ready := mm.req_ready
    dcm.read_req_ready := mm.req_ready && !icm.read_req_valid

    mm.req_valid := icm.read_req_valid || dcm.read_req_valid
    mm.req_rw := Mux(icm.read_req_valid, false.B, dcm.read_req_rw)
    mm.req_addr := Mux(icm.read_req_valid, icm.read_req_addr, dcm.read_req_addr)
    mm.req_tag := Mux(icm.read_req_valid, 0.U, 1.U)

    icm.read_resp_valid := mm.resp_valid && (mm.resp_tag === 0.U)
    dcm.read_resp_valid := mm.resp_valid && (mm.resp_tag === 1.U)
  }
}