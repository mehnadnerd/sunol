package sunol

import chisel3._
import chisel3.util.{Cat, Fill, MuxLookup}

// imem to core
class SunolIMemCoreIO extends Bundle {
  val addr = Input(UInt(32.W))
  val re = Input(Bool())

  val ready = Input(Bool())
  val valid = Output(Bool())
  val data = Output(UInt(32.W))
  val cancel = Input(Bool())
}

// dmem to core
class SunolDMemCoreIO extends Bundle {
  val addr = Input(UInt(32.W))
  val size = Input(UInt(3.W))
  val re = Input(Bool())
  val wdata = Input(UInt(32.W))
  val we = Input(Bool())

  val ready = Input(Bool())
  val rdata = Output(UInt(32.W))
  val valid = Output(Bool())
  val resp = Output(Bool())
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

  val cache = Module(new SunolCache(64))
  val c = cache.io
  val m = io.mem

  c.cpu_req_valid := io.imem.re
  // := c.cpu_req_rdy //TODO: we just kind of assume this is true
  c.cpu_req_addr := io.imem.addr
  c.cpu_req_data := DontCare
  c.cpu_req_write := 0.U

  io.imem.valid := c.cpu_resp_val
  io.imem.data := c.cpu_resp_data

  m.read_req_valid := c.mem_req_val
  c.mem_req_rdy := m.read_req_ready
  m.read_req_addr := c.mem_req_addr
  m.read_req_rw := c.mem_req_rw =/= 0.U

  m.write_req_data_valid := c.mem_req_data_valid
  c.mem_req_data_rdy := m.write_req_data_ready
  m.write_req_data_bits := c.mem_req_data_bits
  m.write_req_data_mask := c.mem_req_data_mask

  c.mem_resp_valid := m.read_resp_valid
  c.mem_resp_data := m.read_resp_data

  // Our additions
  c.associative := true.B
  c.cancel := io.imem.cancel
}

class SunolDCache extends Module {
  val io = IO(new Bundle {
    val dmem = new SunolDMemCoreIO
    val mem = new SunolMainMemCacheIO
  })

  val cache = Module(new SunolCache(64))
  val c = cache.io
  val m = io.mem

  // Handle writes
  val shift = WireInit(io.dmem.addr(1, 0))
  val sizemask = MuxLookup(io.dmem.size, 15.U(4.W), Array(0.U -> 1.U(4.W), 1.U -> 3.U(4.W))) << shift

  // Handle loads
  val sext = !io.dmem.size(2)

  io.dmem.rdata := c.cpu_resp_data
  when(io.dmem.size(1, 0) === 0.U) {
    io.dmem.rdata := Cat(Fill(24, sext & c.cpu_resp_data(7)), c.cpu_resp_data(7, 0))
    when(io.dmem.addr(1, 0) === 1.U) {
      io.dmem.rdata := Cat(Fill(24, sext & c.cpu_resp_data(15)), c.cpu_resp_data(15, 8))
    }.elsewhen(io.dmem.addr(1, 0) === 2.U) {
      io.dmem.rdata := Cat(Fill(24, sext & c.cpu_resp_data(23)), c.cpu_resp_data(23, 16))
    }.elsewhen(io.dmem.addr(1, 0) === 3.U) {
      io.dmem.rdata := Cat(Fill(24, sext & c.cpu_resp_data(31)), c.cpu_resp_data(31, 24))
    }
  }.elsewhen(io.dmem.size(1, 0) === 1.U) {
    io.dmem.rdata := Cat(Fill(16, sext & c.cpu_resp_data(15)), c.cpu_resp_data(15, 0))
    when(io.dmem.addr(1, 0) === 2.U) {
      io.dmem.rdata := Cat(Fill(16, sext & c.cpu_resp_data(31)), c.cpu_resp_data(31, 16))
    }
  }

  c.cpu_req_valid := io.dmem.re
  // := c.cpu_req_rdy //TODO: we just kind of assume this is true
  c.cpu_req_addr := io.dmem.addr
  c.cpu_req_data := io.dmem.wdata << (shift << 3.U).asUInt()
  c.cpu_req_write := Cat(Seq.fill(4)(io.dmem.we)) & sizemask.asUInt()

  io.dmem.valid := c.cpu_resp_val

  io.dmem.rdata := c.cpu_resp_data

  m.read_req_valid := c.mem_req_val
  c.mem_req_rdy := m.read_req_ready
  m.read_req_addr := c.mem_req_addr
  m.read_req_rw := c.mem_req_rw =/= 0.U

  m.write_req_data_valid := c.mem_req_data_valid
  c.mem_req_data_rdy := m.write_req_data_ready
  m.write_req_data_bits := c.mem_req_data_bits
  m.write_req_data_mask := c.mem_req_data_mask

  c.mem_resp_valid := m.read_resp_valid
  c.mem_resp_data := m.read_resp_data

  // Our additions
  c.associative := true.B
  io.dmem.resp := c.cpu_resp_val
  c.cancel := false.B
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