import chisel3._
import chisel3.util._

class SingleCycleRiscV extends Module {
  val io = IO(new Bundle {
    val regDeb = Output(Vec(32, UInt(32.W))) //debug output for the tester
    val regDeb0 = Output(Vec(32, UInt(8.W))) //least significant byte of each register for binary dump
    val regDeb1 = Output(Vec(32, UInt(8.W))) // ...
    val regDeb2 = Output(Vec(32, UInt(8.W))) // ...
    val regDeb3 = Output(Vec(32, UInt(8.W))) //most significant byte for binary dump
    val done = Output(Bool())
    val imemDeb = Output(Vec(109, SInt(32.W))) //debug output for the tester
    val pcDeb = Output(UInt(32.W)) //debug output for the tester
    //val memDeb = Output(UInt(32.W)) //debug output for the tester
  })

  //necessary io initializations
  io.done := false.B
  for (i <- 0 until 109) io.imemDeb(i) := 0.S

  //load machine code from binary file using CopyBytes
  val program = CopyBytes("tests/additional/tasks/t11.bin")
  val imem = VecInit(program.map(_.S(32.W)))  //map into instruction memory as chisel vec

  val vec = Wire(Vec(32, UInt(32.W)))
  for (i <- 0 until 32) vec(i) := 0.U //initialize the register file to 0 for a nicer display
  val reg = RegInit(vec)

  //lots of val declarations for better overview in switch table
  val pc = RegInit(0.U(32.W))
  val instr = imem(pc(31, 2)) //byte offset
  val opcode = instr(6, 0)
  val rd = instr(11, 7)
  val rs1 = instr(19, 15)
  val rs2 = instr(24, 20)
  val funct3 = instr(14, 12)
  val funct7 = instr(31 ,25)
  //immediates are first defined as words
  val imm = WireInit(0.U (32.W)) //I-type immediate
  val Bimm = WireInit(0.U (32.W)) //B-type immediate
  val Simm = WireInit(0.U (32.W)) //S-type immediate
  val Jimm = WireInit(0.U (32.W)) //J-type immediate
  val Uimm = WireInit(0.U (32.W)) //U-type immediate
  Uimm := instr(31, 12) << 12 //Uimm(11, 0) = 0x00
  val sign = instr(31) //for sign-extension
  when (sign) { //sign extension of immediates
    imm := Cat(0xFFFFF.U, instr(31, 20))
    Bimm := Cat(0xFFFFF.U, instr(31),instr(7),instr(30,25),instr(11,8),"b0".U)
    Simm := Cat(0xFFFFF.U, instr(31,25),instr(11,7))
    Jimm := Cat(0xFFFFF.U, instr(31), instr(19, 12), instr(20), instr(30, 21), "b0".U)
  } otherwise {
    imm := instr(31, 20)
    Bimm := Cat(instr(31),instr(7),instr(30,25),instr(11,8),"b0".U)
    Simm := Cat(instr(31,25),instr(11,7))
    Jimm := Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), "b0".U)
  }
  val shift = WireInit(0.U (5.W))

  //memory implementation
  val writeAddr = reg(rs1) + Simm
  val readAddr = reg(rs1) + imm
  val dataIn = reg(rs2)
  val mem = Mem(1048576, UInt(8.W)) //1MB

  //if no more instructions, set io.done = true
  when(pc(31,2) === program.length.asUInt()) {
    io.done := true.B
  }

  pc := pc + 4.U

  //Decoding of instruction and execution
  switch(opcode) {
    //____________________________________R-type_________________________
    is(0x33.U) {
      switch(funct3) {
        is(0x0.U) {
          switch(funct7) {
            is(0x00.U) { //add
              reg(rd) := reg(rs1) + reg(rs2)
            }
            is(0x20.U) { //sub
              reg(rd) := reg(rs1) - reg(rs2)
            }
          }
        }
        is(0x6.U) { //or
          reg(rd) := reg(rs1) | reg(rs2)
        }
        is(0x7.U) { //and
          reg(rd) := reg(rs1) & reg(rs2)
        }
        is(0x1.U) { //sll
          shift := reg(rs2)
          reg(rd) := reg(rs1) << shift
        }
        is(0x5.U) {
          switch(funct7) {
            is(0x00.U) { //srl
              reg(rd) := reg(rs1) >> reg(rs2)
            }
            is(0x20.U) { //sra
              reg(rd) := (reg(rs1).asSInt >> reg(rs2)).asUInt()
            }
          }
        }
        is(0x2.U) { //slt
          when(reg(rs1)(31) =/= reg(rs2)(31)){
            reg(rd) := reg(rs1) > reg(rs2)
          } otherwise {
            reg(rd) := reg(rs1) < reg(rs2)
          }
        }
        is(0x3.U) { //sltu
          reg(rd) := reg(rs1) < reg(rs2)
        }

      }
    }
    //________________________________________I-type_________________________
    is(0x13.U) {
      switch(funct3) {
        is(0x0.U) { //addi
          reg(rd) := reg(rs1) + imm
        }
        is(0x4.U) { //xori
          reg(rd) := reg(rs1) ^ imm
        }
        is(0x6.U) { //ori
          reg(rd) := reg(rs1) | imm
        }
        is(0x7.U) { //andi
          reg(rd) := reg(rs1) & imm
        }
        is(0x1.U) { //slli
          reg(rd) := reg(rs1) << imm(4, 0)
        }
        is(0x5.U) {
          switch(imm(11,5)) {
            is(0x00.U) { //srli
              reg(rd) := reg(rs1) >> imm(4, 0)
            }
            is(0x20.U) { //srai
              reg(rd) := (reg(rs1).asSInt() >> imm(4, 0)).asUInt()
            }
          }
        }
        is(0x2.U) { //slti
          when(reg(rs1)(31) =/= imm(31)){
            reg(rd) := reg(rs1) > imm
          } otherwise {
            reg(rd) := reg(rs1) < imm
          }
        }
        is(0x3.U) { //sltiu
          reg(rd) := reg(rs1) < imm
        }
      }
    }
    //________________________________________I-type Loads_____________________
    is(0x03.U) {
      switch(funct3) {
        is(0x0.U) { //lb
          when(mem.read(readAddr)(7)) { //Check msb for sign
            reg(rd) := Cat(0xFFFFFF.U, mem.read(readAddr))
          } otherwise {
            reg(rd) := mem.read(readAddr)
          }

        }
        is(0x1.U) { //lh
          when(mem.read(readAddr + 1.U)(7)) { //Check msb for sign
            reg(rd) := Cat(0xFFFF.U,mem.read(readAddr + 1.U), mem.read(readAddr))
          } otherwise {
            reg(rd) := Cat(0x0000.U,mem.read(readAddr + 1.U), mem.read(readAddr))
          }
        }
        is(0x2.U) { //lw
          reg(rd) := Cat(mem.read(readAddr + 3.U),
            mem.read(readAddr + 2.U),
            mem.read(readAddr + 1.U),
            mem.read(readAddr))
        }
        is(0x4.U) { //lbu
          reg(rd) := mem.read(readAddr)
        }
        is(0x5.U) { //lhu
          reg(rd) := Cat(mem.read(readAddr + 1.U), mem.read(readAddr))
        }
      }
    }
    //_______________________________________________S-type________________________
    is(0x23.U) {
      switch(funct3) {
        is(0x0.U) { //sb
          mem.write(writeAddr, dataIn(7, 0))
        }
        is(0x1.U) { //sh
          mem.write(writeAddr, dataIn(7, 0))
          mem.write(writeAddr + 1.U, dataIn(15, 8))
        }
        is(0x2.U) { //sw
          mem.write(writeAddr, dataIn(7, 0))
          mem.write(writeAddr + 1.U, dataIn(15, 8))
          mem.write(writeAddr + 2.U, dataIn(23, 16))
          mem.write(writeAddr + 3.U, dataIn(31, 24))
        }
      }
    }
    //________________________________________________B-type__________________________
    is(0x63.U) {
      switch(funct3) {
        is(0x0.U) { //beq
          when(reg(rs1) === reg(rs2)) {
            pc := pc + Bimm
          }
        }
        is(0x1.U) { //bne
          when(reg(rs1) =/= reg(rs2)) {
            pc := pc + Bimm
          }
        }
        is(0x4.U) { //blt
          when(reg(rs1)(31) =/= reg(rs2)(31)) {
            when(reg(rs1) > reg(rs2)) {
              pc := pc + Bimm
            }
          } otherwise {
            when(reg(rs1) < reg(rs2)) {
              pc := pc + Bimm
            }
          }
        }
        is(0x5.U) { //bge
          when(reg(rs1)(31) =/= reg(rs2)(31)) {
            when(reg(rs1) <= reg(rs2)) {
              pc := pc + Bimm
            }
          } otherwise {
            when(reg(rs1) >= reg(rs2)) {
              pc := pc + Bimm
            }
          }
        }
        is(0x6.U) { //bltu
          when(reg(rs1) < reg(rs2)) {
            pc := pc + Bimm
          }
        }
        is(0x7.U) { //bgeu
          when(reg(rs1) >= reg(rs2)) {
            pc := pc + Bimm
          }
        }
      }
    }
    //__________________________________J-type, U-type, ECALL and lui, auipc___________
    is(0x6F.U) { //J-type jal
      reg(rd) := pc + 4.U
      pc := pc + Jimm
    }
    is(0x67.U) { //I-type jalr
      reg(rd) := pc + 4.U
      pc := reg(rs1) + imm
    }
    is(0x37.U) { //U-type lui
      reg(rd) := Uimm
    }
    is(0x17.U) { //U-type auipc
      reg(rd) := pc + Uimm
    }
    is(0x73.U) { //ecall 10
      switch(reg(10)) {
        is(10.U) {
          io.done := true.B
        }
      }
    }
  }

  reg(0) := 0.U //x0 should always be zero

  // Make the io's file visible to the tester
  io.pcDeb <> pc
  for(i <- 0 until 32) {
    io.regDeb0(i) := reg(i)(7,0)
    io.regDeb1(i) := reg(i)(15,8)
    io.regDeb2(i) := reg(i)(23,16)
    io.regDeb3(i) := reg(i)(31,24)
  }
  io.regDeb <> reg
  for (i <- program.indices) io.imemDeb(i) := imem(i)
  //io.memDeb := Cat(mem.read(8.U), mem.read(9.U), mem.read(10.U), mem.read(11.U))
}