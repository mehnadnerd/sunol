package sunol

import chisel3._
import chisel3.util._

class DataCacheIO extends Bundle {
  val CE = Input(Clock())
  val WEB = Input(Bool())
  val OEB = Input(Bool())
  val CSB = Input(Bool())
  val BYTEMASK = Input(UInt(16.W))
  val A = Input(UInt(8.W))
  val I = Input(UInt(128.W))
  val O = Output(UInt(128.W))
}

class TagCacheIO extends Bundle {
  val CE = Input(Clock())
  val WEB = Input(Bool())
  val OEB = Input(Bool())
  val CSB = Input(Bool())
  val A = Input(UInt(6.W))
  val I = Input(UInt(32.W))
  val O = Output(UInt(32.W))
}

// This gives us a max of 256 rows
class SRAM1RW256x128 extends BlackBox {
  val io = IO(new DataCacheIO)
}

// This gives us a max of 256 rows
class SRAM1RW64x32 extends BlackBox {
  val io = IO(new TagCacheIO)
}

class SunolCache(rows: Int) extends Module {
  // Constants
  val LINE_LENGTH_BITS = 2
  val ROWS_BITS = log2Ceil(rows)
  val TAG_TO_DATA_SCALING_FACTOR = 512/128
  val SCALING_FACTOR_BITS = log2Ceil(TAG_TO_DATA_SCALING_FACTOR)
  val BYTES_IN_SRAM_ROW = 128 / 8
  val BYTES_IN_SRAM_ROW_BITS = log2Ceil(BYTES_IN_SRAM_ROW)
  val ADDR_BITS = 30 - ROWS_BITS - LINE_LENGTH_BITS - SCALING_FACTOR_BITS

  println(s"Line length: $LINE_LENGTH_BITS")
  println(s"Row bits: $ROWS_BITS")
  println(s"Scaling factor bits: $SCALING_FACTOR_BITS")
  println(s"Addr bits: $ADDR_BITS")

  val io = IO(new Bundle {
    val cpu_req_valid = Input(Bool())
    val cpu_req_rdy = Output(Bool())
    val cpu_req_addr = Input(UInt(30.W))
    val cpu_req_data = Input(UInt(32.W))
    val cpu_req_write = Input(UInt(4.W))

    val cpu_resp_val = Output(Bool())
    val cpu_resp_data = Output(UInt(32.W))

    val mem_req_val = Output(Bool())
    val mem_req_rdy = Input(Bool())
    val mem_req_addr = Output(UInt((30 - LINE_LENGTH_BITS).W))
    val mem_req_rw = Output(UInt(1.W))
    val mem_req_data_valid = Output(Bool())
    val mem_req_data_rdy = Input(Bool())
    val mem_req_data_bits = Output(UInt(128.W))
    val mem_req_data_mask = Output(UInt(16.W))

    val mem_resp_val = Input(Bool())
    val mem_resp_data = Input(UInt(128.W))

    // Our additions
    val cpu_resp_addr = Output(UInt(30.W))
    val associative = Input(Bool())
    val cancel = Input(Bool())
  })

  // Methods
  def isValid(tag: UInt): Bool = {
    tag(tag.getWidth - 1)
  }

  def isDirty(tag: UInt): Bool = {
    tag(tag.getWidth - 2)
  }

  def isHit(tag: UInt, addr: UInt): Bool = {
    isValid(tag) && addrMetadataInTag(tag) === addrMetadataInCpuAddr(addr)
  }

  def addrMetadataInTag(tag: UInt): UInt = {
    tag(ADDR_BITS - 1, 0)
  }

  def addrMetadataInCpuAddr(addr: UInt) = {
    require(addr.getWidth == 30)
    addr(addr.getWidth-1, addr.getWidth-ADDR_BITS)
  }

  def cpuAddrToTagCacheAddr(cpu_addr: UInt): UInt = {
    cpu_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS)
  }

  def cpuAddrToDataCacheAddr(cpu_addr: UInt): UInt = {
    cpu_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS)
  }

  // State
  val idle :: check :: w1 :: w2 :: w3 :: w4 :: r0 :: r1 :: r2 :: r3 :: r4 :: done :: cancel :: Nil = Enum(13)
  val state = RegInit(idle)

  val old_resp = RegInit(io.cpu_resp_data)
  old_resp := io.cpu_resp_data

  val old_req_addr = Reg(UInt(io.cpu_req_addr.getWidth.W))
  val old_req_data = Reg(UInt(io.cpu_req_data.getWidth.W))
  val old_req_write = Reg(UInt(io.cpu_req_write.getWidth.W))

  // Memories
  val data_way = Seq(Module(new SRAM1RW256x128()), Module(new SRAM1RW256x128()))
  val tags_way = Seq(Module(new SRAM1RW64x32()), Module(new SRAM1RW64x32()))

  // Wires
  val cpu_req_fire = io.cpu_req_rdy && io.cpu_req_valid
  val mem_req_fire = io.mem_req_rdy && io.mem_req_val
  val mem_req_data_fire = io.mem_req_data_rdy && io.mem_req_data_valid

  val hit = Wire(Bool())
  hit := tags_way.map(tw => isHit(tw.io.O, old_req_addr)).reduce(_ || _)
  val hit_data = Wire(UInt(128.W))
  hit_data := Mux(isHit(tags_way(0).io.O, old_req_addr), data_way(0).io.O, data_way(1).io.O)

  val evict = Wire(Bool())
  evict := hit && tags_way.map(tw => isValid(tw.io.O) && isDirty(tw.io.O)).reduce(_ && _)

  val rand = LFSR16()(0)

  val eviction_target = Wire(UInt(1.W))
  eviction_target := Mux(!isValid(tags_way(0).io.O), 0.U, Mux(!isValid(tags_way(1).io.O), 1.U, rand)) & io.associative

  val eviction_tag_cache_io = Wire(new TagCacheIO)
  val eviction_data_cache_io = Wire(new DataCacheIO)

  when (eviction_target === 0.U) {
    eviction_tag_cache_io := tags_way(0).io
    eviction_data_cache_io := data_way(0).io
  }.otherwise {
    eviction_tag_cache_io := tags_way(1).io
    eviction_data_cache_io := data_way(1).io
  }

  // Debug
  val debug_tag_cache0_addr = WireInit(addrMetadataInTag(tags_way(0).io.O))
  val debug_tag_cache1_addr = WireInit(addrMetadataInTag(tags_way(1).io.O))
  val debug_addr_addr = WireInit(addrMetadataInCpuAddr(io.cpu_req_addr))

  val debug_tag_row_num = WireInit(cpuAddrToTagCacheAddr(io.cpu_req_addr))
  val debug_data_row_num = WireInit(cpuAddrToDataCacheAddr(io.cpu_req_addr))

  chisel3.core.dontTouch(debug_tag_cache0_addr)
  chisel3.core.dontTouch(debug_tag_cache1_addr)
  chisel3.core.dontTouch(debug_addr_addr)
  chisel3.core.dontTouch(debug_tag_row_num)
  chisel3.core.dontTouch(debug_data_row_num)

  // Next state logic
  val next_state = WireInit(idle)
  state := next_state

  next_state := idle

  switch (state) {
    is (idle) {
      when (cpu_req_fire) {
        next_state := check
      }
    }

    is (check) {
      when (hit) {
        next_state := Mux((old_req_write === 0.U) && cpu_req_fire, check, idle)
      }.elsewhen (evict) {
        next_state := Mux(mem_req_data_fire, w1, check)
      }.otherwise {
        // next_state := r0
        next_state := Mux(mem_req_fire, r1, r0)
      }
    }

    is (r0) {
      next_state := Mux(mem_req_fire, r1, r0)
    }

    is (done) {
      next_state := Mux(RegNext(state) === done, idle, done)
    }

    is (cancel) {
      next_state := Mux(RegNext(state) === cancel, check, cancel) // TODO can we save one more cycle here?
    }
  }

  when (state >= w1 && state <= w4) {
    next_state := Mux(mem_req_data_fire, state + 1.U, state)
  }

  when (state >= r1 && state <= r4) {
    next_state := state + 1.U
  }

  when (io.cancel) {
    next_state := cancel
  }

  // Output logic
  io.cpu_req_rdy := false.B // Bool()

  io.cpu_resp_data := old_resp

  io.cpu_resp_val := false.B // Bool()

  io.mem_req_val := false.B // Bool()
  io.mem_req_addr := DontCare // UInt(8.W)
  io.mem_req_rw := 0.U /* read */ // UInt(1.W)
  io.mem_req_data_valid := false.B // Bool()
  io.mem_req_data_bits := DontCare // UInt(128.W)
  io.mem_req_data_mask := 0.U // UInt(16.W)

  io.cpu_resp_addr := old_req_addr

  data_way.foreach { d =>
    d.io.CE := clock
    d.io.OEB := 0.U
    d.io.CSB := 0.U

    // Defaults
    d.io.A := cpuAddrToDataCacheAddr(old_req_addr)
    d.io.WEB := 1.U // read
    d.io.BYTEMASK := 0.U
    d.io.I := DontCare
  }

  tags_way.foreach { t =>
    t.io.CE := clock
    t.io.OEB := 0.U
    t.io.CSB := 0.U

    // Defaults
    t.io.A := cpuAddrToTagCacheAddr(old_req_addr)
    t.io.WEB := 1.U // read
    t.io.I := DontCare
  }

  switch (state) {
    is (idle) {
      io.cpu_req_rdy := true.B

      old_req_addr := io.cpu_req_addr
      old_req_data := io.cpu_req_data
      old_req_write := io.cpu_req_write

      data_way.foreach( _.io.A := cpuAddrToDataCacheAddr(io.cpu_req_addr) )
      tags_way.foreach( _.io.A := cpuAddrToTagCacheAddr(io.cpu_req_addr) )
    }

    is (check) {
      when (hit) {
        io.cpu_resp_val := true.B
        io.cpu_resp_addr := old_req_addr
        io.cpu_resp_data := hit_data >> (old_req_addr(1, 0) << 5).asUInt()

        // Handle stores
        for (i <- 0 to 1) {
          when((i.U === eviction_target) && (old_req_write =/= 0.U)) {
            val position_on_line = (old_req_addr(LINE_LENGTH_BITS-1, 0) << 5).asUInt()
            val modified_data = hit_data & (~((-1).S(32.W) << position_on_line)).asUInt() | (old_req_data << position_on_line).asUInt()

            data_way(i).io.I := modified_data
            data_way(i).io.BYTEMASK := old_req_write << (position_on_line >> 3).asUInt()
            data_way(i).io.WEB := 0.U // Write

            // Mark tag as dirty
            tags_way(i).io.I := Cat(1.U(1.W), 1.U(1.W), Fill(30 - ADDR_BITS, 0.U(1.W)), addrMetadataInCpuAddr(old_req_addr))
            tags_way(i).io.WEB := 0.U // write
          }
        }

        // If there is no store, then just go straight back to this cycle
        // TODO can we save a cycle on stores as well?
        when (old_req_write === 0.U) {
          io.cpu_req_rdy := true.B

          old_req_addr := io.cpu_req_addr
          old_req_data := io.cpu_req_data
          old_req_write := io.cpu_req_write

          data_way.foreach(_.io.A := cpuAddrToDataCacheAddr(io.cpu_req_addr))
          tags_way.foreach(_.io.A := cpuAddrToTagCacheAddr(io.cpu_req_addr))
        }
      }.elsewhen(evict) {
        // Set dcache pointer to 0
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), 0.U(SCALING_FACTOR_BITS)))
      }.otherwise {
        // Initialize MEM pointer to 0
        io.mem_req_addr := Cat(old_req_addr(old_req_addr.getWidth - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), Fill(SCALING_FACTOR_BITS, 0.U))

        when (io.mem_req_rdy) {
          io.mem_req_val := true.B
        }

        // Invalidate current cacheline
        for (i <- 0 to 1) {
          when(i.U === eviction_target) {
            tags_way(i).io.I := Fill(32, 0.U(1.W))
            tags_way(i).io.WEB := 0.U // write
          }
        }
      }
    }

    is (w1) {
      val offset = 0

      // Write cached data into memory
      io.mem_req_val := true.B
      io.mem_req_data_valid := true.B
      io.mem_req_addr := Cat(addrMetadataInTag(eviction_tag_cache_io.O), old_req_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
      io.mem_req_rw := 1.U
      io.mem_req_data_mask := (-1).S(16.W).asUInt()
      io.mem_req_data_bits := eviction_data_cache_io.O

      // Increment dcache pointer
      when (next_state =/= w1) {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), (offset+1).U(SCALING_FACTOR_BITS)))
      }.otherwise {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS)))
      }
    }

    is (w2) {
      val offset = 1

      // Write cached data into memory
      io.mem_req_val := true.B
      io.mem_req_data_valid := true.B
      io.mem_req_addr := Cat(addrMetadataInTag(eviction_tag_cache_io.O), old_req_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
      io.mem_req_rw := 1.U
      io.mem_req_data_mask := (-1).S(16.W).asUInt()
      io.mem_req_data_bits := eviction_data_cache_io.O

      // Increment dcache pointer
      when (next_state =/= w2) {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), (offset+1).U(SCALING_FACTOR_BITS)))
      }.otherwise {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS)))
      }
    }

    is (w3) {
      val offset = 2

      // Write cached data into memory
      io.mem_req_val := true.B
      io.mem_req_data_valid := true.B
      io.mem_req_addr := Cat(addrMetadataInTag(eviction_tag_cache_io.O), old_req_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
      io.mem_req_rw := 1.U
      io.mem_req_data_mask := (-1).S(16.W).asUInt()
      io.mem_req_data_bits := eviction_data_cache_io.O

      // Increment dcache pointer
      when (next_state =/= w3) {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), (offset+1).U(SCALING_FACTOR_BITS)))
      }.otherwise {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS)))
      }
    }

    is (w4) {
      val offset = 3

      // Write cached data into memory
      io.mem_req_val := true.B
      io.mem_req_data_valid := true.B
      io.mem_req_addr := Cat(addrMetadataInTag(eviction_tag_cache_io.O), old_req_addr(LINE_LENGTH_BITS + SCALING_FACTOR_BITS + ROWS_BITS - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
      io.mem_req_rw := 1.U
      io.mem_req_data_mask := (-1).S(16.W).asUInt()
      io.mem_req_data_bits := eviction_data_cache_io.O

      // Increment dcache pointer
      when (next_state =/= w4) {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), (offset+1).U(SCALING_FACTOR_BITS)))
      }.otherwise {
        data_way.foreach(_.io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS)))
      }
    }

    is (r0) {
      // Initialize MEM pointer to 0
      io.mem_req_addr := Cat(old_req_addr(old_req_addr.getWidth - 1, LINE_LENGTH_BITS + SCALING_FACTOR_BITS), Fill(SCALING_FACTOR_BITS, 0.U))

      when (io.mem_req_rdy) {
        io.mem_req_val := true.B
      }

      // Invalidate current cacheline
      for (i <- 0 to 1) {
        when(i.U === eviction_target) {
          tags_way(i).io.I := Fill(32, 0.U(1.W))
          tags_way(i).io.WEB := 0.U // write
        }
      }
    }

    is (r1) {
      val offset = 0

      // Read from MEM into cached data
      for (i <- 0 to 1) {
        when (i.U === eviction_target) {
          data_way(i).io.I := io.mem_resp_data
          data_way(i).io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
          data_way(i).io.WEB := 0.U // 0.U // write
          data_way(i).io.BYTEMASK := (-1).S(16.W).asUInt()
        }
      }
    }

    is (r2) {
      val offset = 1

      // Read from MEM into cached data
      for (i <- 0 to 1) {
        when (i.U === eviction_target) {
          data_way(i).io.I := io.mem_resp_data
          data_way(i).io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
          data_way(i).io.WEB := 0.U // 0.U // write
          data_way(i).io.BYTEMASK := (-1).S(16.W).asUInt()
        }
      }
    }

    is (r3) {
      val offset = 2

      // Read from MEM into cached data
      for (i <- 0 to 1) {
        when (i.U === eviction_target) {
          data_way(i).io.I := io.mem_resp_data
          data_way(i).io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
          data_way(i).io.WEB := 0.U // 0.U // write
          data_way(i).io.BYTEMASK := (-1).S(16.W).asUInt()
        }
      }
    }

    is (r4) {
      val offset = 3

      // Read from MEM into cached data
      for (i <- 0 to 1) {
        when (i.U === eviction_target) {
          data_way(i).io.I := io.mem_resp_data
          data_way(i).io.A := Cat(cpuAddrToDataCacheAddr(old_req_addr)(ROWS_BITS + SCALING_FACTOR_BITS - 1, SCALING_FACTOR_BITS), offset.U(SCALING_FACTOR_BITS.W))
          data_way(i).io.WEB := 0.U // 0.U // write
          data_way(i).io.BYTEMASK := (-1).S(16.W).asUInt()

          // Update tags
          tags_way(i).io.I := Cat(1.U(1.W), 0.U(1.W), Fill(30 - ADDR_BITS, 0.U(1.W)), addrMetadataInCpuAddr(old_req_addr))
          tags_way(i).io.WEB := 0.U // write
        }
      }
    }

    is (done) {
      // TODO go straight back to check from this stage to save a cycle

      // TODO get rid of RegNext. Right now, we wait to make sure the memory is read out properly
      when (RegNext(state) === done) {
        for (i <- 0 to 1) {
          when((i.U === eviction_target) && (old_req_write =/= 0.U)) {
            val position_on_line = (old_req_addr(LINE_LENGTH_BITS-1, 0) << 5).asUInt()
            val modified_data = hit_data & (~((-1).S(32.W) << position_on_line)).asUInt() | (old_req_data << position_on_line).asUInt()

            data_way(i).io.I := modified_data
            data_way(i).io.BYTEMASK := old_req_write << (position_on_line >> 3).asUInt()
            data_way(i).io.WEB := 0.U // Write

            // Mark tag as dirty
            tags_way(i).io.I := Cat(1.U(1.W), 1.U(1.W), Fill(30 - ADDR_BITS, 0.U(1.W)), addrMetadataInCpuAddr(old_req_addr))
            tags_way(i).io.WEB := 0.U // write
          }
        }

        io.cpu_resp_val := true.B
        io.cpu_resp_data := hit_data >> (old_req_addr(1, 0) << 5).asUInt()
      }
    }

    is (cancel) {
      when (io.cpu_req_valid) {
        old_req_addr := io.cpu_req_addr
      }
    }
  }
}
