package sunol

import chisel3._
import chisel3.util._
import sunol.Util._
import sunol.Constants._

class SunolCore extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new SunolIMemIO())
    val dmem = Flipped(new SunolDMemIO())
    val tohost = Output(UInt(32.W))
  })

  val pc = RegInit(0x2000.U(32.W))
  val regfile = Reg(Vec(32, UInt(32.W))) // TODO: handle x0
  def rf(num: UInt): UInt = Mux(num === 0.U, 0.U, regfile(num))

  val csr = RegInit(0.U(32.W))
  io.tohost := csr

  val branch_taken = Wire(Bool()) // TODO: do this better
  val branch_addr = Wire(UInt(32.W))

  // register declarations

  //instruction fetch inputs
  //val if_pc = Reg(UInt(32.W)) // pc to fetch instruction at //TODO: should this just be pc?
  val if_pc_valid = Reg(Bool()) // whether this pc is valid and instruction
  //decode
  val de_ready = Wire(Bool())
  val de_valid = RegInit(false.B) // whether instruction is valid
  val de_pc = Reg(UInt(32.W))
  val de_inst = Reg(RVInstruction()) // instruction to be decoded

  //execute
  val ex_ready = Wire(Bool())
  val ex_valid = RegInit(false.B) // whether the things to execute are valid
  val ex_pc = Reg(UInt(32.W))

  val ex_rs1 = Reg(UInt(32.W)) // rs1
  val source1_rs1 :: source1_pc :: source1_zero :: Nil = Enum(3)
  val ex_op1source = Reg(UInt(2.W)) // whether op1 comes from rs1 or something else
  val ex_rs2 = Reg(UInt(32.W)) // rs2
  val source2_rs2 :: source2_imm :: Nil = Enum(2)
  val ex_op2source = Reg(UInt(1.W)) // whether op2 comes from rs1 or something else
  val ex_imm = Reg(UInt(32.W)) // imm
  val ex_rd_num = Reg(UInt(5.W)) // rd num
  val ex_rs1_num = Reg(UInt(5.W)) // for bypassing
  val ex_rs2_num = Reg(UInt(5.W)) // for bypassing
  val ex_alu_funct = Reg(UInt(3.W)) // alu funct
  val ex_alu_add_arith = Reg(Bool()) // alu add/sub arithmetic/logical

  val ex_b_ctrl = Reg(UInt(3.W))
  val ex_b_use = Reg(Bool()) // whether or not should evaluate branch condition
  val ex_j = Reg(Bool())

  //passthrough things
  val ex_mem_width = Reg(UInt(3.W)) // memwidth, passthroiugh
  val ex_mem_re = Reg(Bool())
  val ex_mem_we = Reg(Bool())


  val wbs_alu :: wbs_mem :: wbs_pc4 :: Nil = Enum(3)
  val ex_wb_src = Reg(UInt(2.W))
  val ex_wb_en = Reg(Bool())

  //mem
  val me_ready = Wire(Bool())
  val me_valid = RegInit(false.B) //mem things are valid
  val me_pc4 = Reg(UInt(32.W))


  val me_alu_out = Reg(UInt(32.W)) // alu output
  val me_width = Reg(UInt(3.W)) // width
  //val me_addr = Reg(UInt(32.W)) // addr for mem //TODO: might be same as aluout
  val me_wdata = Reg(UInt(32.W)) // data to be written -- always rs2?
  val me_re = Reg(Bool()) // read enable
  val me_we = Reg(Bool()) // write enable

  val me_wb_src = Reg(UInt(2.W))
  val me_rd_num = Reg(UInt(5.W))
  val me_wb_en = Reg(Bool())

  //writeback
  val wb_ready = Wire(Bool())
  val wb_valid = RegInit(false.B)
  val wb_pc4 = Reg(UInt(32.W))
  val wb_rd_num = Reg(UInt(5.W)) // reg to writeback to
  val wb_alu = Reg(UInt(32.W))
  val wb_mem = Reg(UInt(32.W))
  val wb_en = Reg(Bool()) //
  val wb_src = Reg(UInt(2.W))

  //updates - datapath
  //instruction fetch
  {
    io.imem.addr := pc
    io.imem.re := true.B
    when(RegNext(if_pc_valid) && de_ready && !RegNext(branch_taken)) {
      de_inst := io.imem.data.asTypeOf(RVInstruction())
      de_valid := io.imem.resp
      de_pc := RegNext(pc)
    }.otherwise {
      when(ex_ready) { //decode is done whenever ex accepts it
        de_valid := false.B
      }
    }
  }
  //decode
  {
    de_ready := ex_ready || !de_valid
    when(de_valid && ex_ready) {
      val opcode = de_inst.full(6, 0)

      val rs1_num = de_inst.full(19, 15)
      val rs2_num = de_inst.full(24, 20)
      val rd_num = de_inst.full(11, 7)
      ex_rs1_num := rs1_num
      ex_rs2_num := rs2_num
      ex_rd_num := rd_num

      ex_rs1 := rf(rs1_num)
      ex_rs2 := rf(rs2_num)

      ex_alu_add_arith := Mux(opcode === OPCODE_OP_IMM.U && de_inst.full(14, 12) === 0.U, false.B, de_inst.full(30)) //complicated b/c addi
      ex_alu_funct := de_inst.full(14, 12)

      ex_mem_width := de_inst.full(14, 12)

      ex_b_ctrl := de_inst.full(14, 12)

      ex_pc := de_pc

      //defaults
      ex_imm := DontCare
      ex_op1source := DontCare
      ex_op2source := DontCare
      ex_mem_re := false.B
      ex_mem_we := false.B
      ex_wb_src := wbs_alu
      ex_wb_en := false.B
      ex_b_use := false.B
      ex_j := false.B

      //imm decode
      {
        val imm_i = Cat(Fill(21, de_inst.full(31)), de_inst.full(30, 20))
        val imm_s = Cat(Fill(21, de_inst.full(31)), de_inst.full(30, 25), de_inst.full(11, 8), de_inst.full(7))
        val imm_b = Cat(Fill(20, de_inst.full(31)), de_inst.full(7), de_inst.full(30, 25), de_inst.full(11, 8), 0.U(1.W))
        val imm_u = Cat(de_inst.full(31, 12), Fill(12, 0.U(1.W)))
        val imm_j = Cat(Fill(12, de_inst.full(31)), de_inst.full(19, 12), de_inst.full(20), de_inst.full(30, 21), 0.U(1.W))

        switch(opcode) {
          is(OPCODE_OP_IMM.U) {
            ex_imm := imm_i
            ex_op1source := source1_rs1
            ex_op2source := source2_imm
            ex_wb_en := true.B
          }
          is(OPCODE_OP.U) {
            ex_op1source := source1_rs1
            ex_op2source := source2_rs2
            ex_wb_en := true.B
          }
          is(OPCODE_AUIPC.U) {
            ex_imm := imm_u
            ex_op1source := source1_pc
            ex_op2source := source2_imm
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also addd
            ex_wb_en := true.B
          }
          is(OPCODE_LUI.U) {
            ex_imm := imm_u
            ex_rs1 := 0.U
            //ex_rs1_num := 0.U
            ex_op1source := source1_zero // TODO: fix this hack
            ex_op2source := source2_imm
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also addd
            ex_wb_en := true.B
          }
          is(OPCODE_LOAD.U) {
            ex_imm := imm_i
            ex_op1source := source1_rs1
            ex_op2source := source2_imm
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also addd
            ex_mem_re := true.B
            ex_wb_en := true.B
            ex_wb_src := wbs_mem
          }
          is(OPCODE_STORE.U) {
            ex_imm := imm_s
            ex_op1source := source1_rs1
            ex_op2source := source2_imm // using alu for address calculation b/c is easier
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also addd
            ex_mem_we := true.B
          }
          is(OPCODE_BRANCH.U) {
            ex_imm := imm_b
            ex_b_use := true.B
            ex_op1source := source1_pc
            ex_op2source := source2_imm
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also addd
          }
          is(OPCODE_JAL.U) {
            ex_imm := imm_j
            ex_op1source := source1_pc
            ex_op2source := source2_imm // using alu for jump
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also add
            ex_wb_en := true.B
            ex_wb_src := wbs_pc4
            ex_j := true.B
          }
          is(OPCODE_JALR.U) {
            ex_imm := imm_i
            ex_op1source := source1_rs1
            ex_op2source := source2_imm // using alu for jump
            ex_alu_funct := 0.U // add
            ex_alu_add_arith := 0.U //also add
            ex_wb_en := true.B
            ex_wb_src := wbs_pc4
            ex_j := true.B
          }
          is(OPCODE_SYSTEM.U) { // only system supporting is cssrw/i
            when(de_inst.full(14) === 1.U) {
              csr := de_inst.full(19, 15)
            }.otherwise {
              csr := rf(de_inst.full(19, 15))
            }
            ex_imm := DontCare
            ex_op1source := DontCare
            ex_op2source := DontCare
            ex_alu_funct := DontCare
            ex_alu_add_arith := DontCare
          }
          is(OPCODE_MISC_MEM.U) {
            ex_imm := DontCare
            ex_op1source := DontCare
            ex_op2source := DontCare
            ex_alu_funct := DontCare
            ex_alu_add_arith := DontCare
          }
        }
      }
      ex_valid := true.B
    }.otherwise {
      when(me_ready) { // this is when ex will be done
        ex_valid := false.B // make sure not invalidate when shouldn't
      }

    }
  }

  //execute
  ex_ready := !ex_valid || me_ready

  val alu_out = Wire(UInt(32.W))
  //alu
  val op1 = Mux(ex_op1source === source1_rs1, ex_rs1, Mux(ex_op1source === source1_pc, ex_pc, 0.U))
  val op2 = Mux(ex_op2source === source2_rs2, ex_rs2, ex_imm)
  alu_out := DontCare
  switch(ex_alu_funct) {
    is(0.U) {
      alu_out := Mux(ex_alu_add_arith, (op1.asSInt() - op2.asSInt()).asUInt(), op1 + op2)
    }
    is(1.U) {
      alu_out := op1 << op2(4, 0)
    }
    is(2.U) {
      alu_out := op1.asSInt() < op2.asSInt()
    }
    is(3.U) {
      alu_out := op1 < op2
    }
    is(4.U) {
      alu_out := op1 ^ op2
    }
    is(5.U) {
      alu_out := Mux(ex_alu_add_arith, (op1.asSInt() >> op2(4, 0)).asUInt(), op1 >> op2(4, 0))
    }
    is(6.U) {
      alu_out := op1 | op2
    }
    is(7.U) {
      alu_out := op1 & op2
    }
  }

  //branching
  {
    val branch_invert = ex_b_ctrl(0)
    val branch_signed = !ex_b_ctrl(1)
    val branch_lt_eq = ex_b_ctrl(2)

    val preinvert = Mux(branch_lt_eq, Mux(branch_signed, ex_rs1.asSInt() < ex_rs2.asSInt(), ex_rs1 < ex_rs2), ex_rs1 === ex_rs2)
    val inverted = preinvert ^ branch_invert
    branch_taken := ((inverted && ex_b_use) || ex_j) && ex_valid && me_ready
  }

  when(ex_valid && me_ready) { //TODO: added me_ready to this so don't lose link when jumping


    branch_addr := alu_out
    when(me_ready) {
      me_alu_out := alu_out
    }

    when(me_ready) {
      me_width := ex_mem_width
      me_re := ex_mem_re
      me_we := ex_mem_we
      me_pc4 := ex_pc + 4.U
      me_wdata := ex_rs2
      me_rd_num := ex_rd_num
      me_wb_en := ex_wb_en
      me_wb_src := ex_wb_src
      me_valid := true.B
    }
  }.otherwise {
    when(!me_re || io.dmem.resp) { // after doing thing with side effects, stop doing it again?? TODO: is this better
      me_valid := false.B
    }
    //me_valid := false.B // TODO: can this result in invaliding something in the mem stage that shouldn't be? hopefully not
    branch_taken := false.B
    branch_addr := 0.U
  }


  //mem
  {
    val mem = io.dmem
    me_ready := !me_valid || ((mem.resp && me_alu_out === RegNext(me_alu_out) && me_re === RegNext(me_re)) || !me_re)
    mem.re := me_re && (me_valid && wb_ready)
    mem.size := me_width
    mem.addr := me_alu_out
    mem.wdata := me_wdata
    mem.we := me_we && (me_valid && wb_ready)
    when(me_valid && wb_ready) {

      wb_mem := mem.rdata

      wb_valid := ((mem.resp && me_alu_out === RegNext(me_alu_out) && me_re === RegNext(me_re)) || !me_re) //assumming writes always take 1 cycle, if not change to !(me_re || me_we)

      wb_alu := me_alu_out
      wb_src := me_wb_src
      wb_pc4 := me_pc4
      wb_rd_num := me_rd_num
      wb_en := me_wb_en
    }.otherwise {
      wb_valid := false.B
    }
  }

  //writeback
  {
    wb_ready := true.B // always ready for writeback
    when(wb_valid) {
      when(wb_en) {
        val wb_val = Mux(wb_src === wbs_alu, wb_alu, Mux(wb_src === wbs_mem, wb_mem, wb_pc4))
        when(wb_rd_num === 0.U) {
          //nothing
        }.otherwise {
          regfile(wb_rd_num) := wb_val
        }
      }
    }
  }

  //bypassing stuff -- out here b/c reverse ordering

  when(me_ready && de_valid && !branch_taken) { // can only do this bypassing when decoding
    when(wb_valid) { //from wb
      //bypassing code
      when(de_inst.full(19, 15) === wb_rd_num && wb_en && wb_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs1 := Mux(wb_src === wbs_alu, wb_alu, Mux(wb_src === wbs_mem, wb_mem, wb_pc4))
      }
      when(de_inst.full(24, 20) === wb_rd_num && wb_en && wb_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs2 := Mux(wb_src === wbs_alu, wb_alu, Mux(wb_src === wbs_mem, wb_mem, wb_pc4))
      }
    }
    when(me_valid) { //from mem
      //bypassing code
      when(de_inst.full(19, 15) === me_rd_num && me_wb_src === wbs_alu && me_wb_en && me_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs1 := me_alu_out
      }
      when(de_inst.full(24, 20) === me_rd_num && me_wb_src === wbs_alu && me_wb_en && me_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs2 := me_alu_out
      }
    }
    when(me_ready && ex_valid) { //from ex
      //bypassing code
      when(de_inst.full(19, 15) === ex_rd_num && ex_wb_src === wbs_alu && ex_wb_en && ex_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs1 := alu_out
      }
      when(de_inst.full(24, 20) === ex_rd_num && ex_wb_src === wbs_alu && ex_wb_en && ex_rd_num =/= 0.U) {
        //rs1 bypass
        ex_rs2 := alu_out
      }
    }
  }

  //control stuff
  {
    //things to do:
    //normal pc+4
    //from alu - this covers branch target addresses and jal/jalr
    when(if_pc_valid && de_ready && io.imem.resp) { // if sending
      pc := pc + 4.U
    }
    when(branch_taken) { // branch or jump
      pc := branch_addr
      //need to kill bad instructions
      de_valid := false.B
      ex_valid := false.B
      if_pc_valid := false.B
    }
    when(!if_pc_valid) {
      if_pc_valid := true.B //delaying pc by one cycle
    }

  }
}

