import chisel3._
import chisel3.util._

/**
  * Author Martin Schoeberl (martin@jopdesign.com)
  *
  * A single-cycle implementation of RISC-V is practically an ISA simulator.
  */
class SingleCycleRiscV extends Module {
  val io = IO(new Bundle {
    val regDeb = Output(Vec(32, UInt(32.W))) // debug output for the tester
    val done = Output(Bool())
    val imemDeb = Output(Vec(109, SInt(32.W)))
    val pcDeb = Output(UInt(32.W))
    val memDeb = Output(UInt(32.W))
  })


  for (i <- 0 until 109) io.imemDeb(i) := 0.S

  val program = CopyBytes("tests/task3/loop.bin") //load machine code from file
  val imem = VecInit(program.map(_.S(32.W)))  //map to chisel vec
  for (i <- program.indices) io.imemDeb(i) := imem(i) //for the tester


/*
  val program = Array[BigInt](
    0x00400513,
    0x00200593,
    0x00b52223,
    0x00452603)

  // A little bit of functional magic to convert the Scala Int Array to a Chisel Vec of UInt
  val imem = VecInit(program.map(_.S(32.W)))
  for (i <- program.indices) io.imemDeb(i) := imem(i) //for the tester

 */


  val pc = RegInit(0.U(32.W))

  // TODO: there should be an elegant way to express this
  val vec = Wire(Vec(32, UInt(32.W)))
  for (i <- 0 until 32) vec(i) := 0.U
  // We initialize the register file to 0 for a nicer display
  // In a real processor this is usually not done
  val reg = RegInit(vec)
  reg(0) := 0.U

  val instr = imem(pc(31, 2))

  val opcode = instr(6, 0)
  val rd = instr(11, 7)
  val rs1 = instr(19, 15)
  val rs2 = instr(24, 20)
  val funct3 = instr(14, 12)
  val funct7 = instr(31 ,25)
  val imm = WireInit(0.U (32.W))
  val Bimm = WireInit(0.U (32.W))
  val Simm = WireInit(0.U (32.W))
  val Jimm = WireInit(0.U (32.W))
  val Uimm = WireInit(0.U (32.W))
  Uimm := instr(31, 12) << 12
  val sign = instr(31)
  when (sign) {                           //sign extension
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
  val sraisign = reg(rs1)
  val shift = WireInit(0.U (5.W)) //kan måske ændres til reg(rs2) direkte

  //memory - 1MB
  val writeAddr = reg(rs1) + Simm
  val readAddr = reg(rs1) + imm
  val dataIn = reg(rs2)
  val mem = Mem(1000000, UInt(8.W))

  pc := pc + 4.U

  switch(opcode) {
    is(0x33.U) {                              //R-type
      switch(funct3) {
        is(0x0.U) {  //add, sub
          switch(funct7) {
            is(0x00.U) {
              reg(rd) := reg(rs1) + reg(rs2) //add
            }
            is(0x20.U) {
              reg(rd) := reg(rs1) - reg(rs2) //sub
            }
          }
        }
        is(0x6.U) {
          reg(rd) := reg(rs1) | reg(rs2) //or
        }
        is(0x7.U) {
          reg(rd) := reg(rs1) & reg(rs2) //and
        }
        is(0x1.U) {     //sll
          shift := reg(rs2)
          reg(rd) := reg(rs1) << shift
        }
        is(0x5.U) {
          switch(funct7) {
            is(0x00.U) {
              reg(rd) := reg(rs1) >> reg(rs2) //srl
            }
            is(0x20.U) {
              reg(rd) := reg(rs1) >> reg(rs2) //sra             ***** mangler msb-extend
            }
          }
        }
        is(0x2.U) {
          when(reg(rs1)(31) =/= reg(rs2)(31)){ //slt
            reg(rd) := reg(rs1) > reg(rs2)
          } otherwise {
            reg(rd) := reg(rs1) < reg(rs2)
          }
        }
        is(0x3.U) {
          reg(rd) := reg(rs1) < reg(rs2) //sltu
        }

      }
    }
    is(0x13.U) {                              //I-type
      switch(funct3) {
        is(0x0.U) {
          reg(rd) := reg(rs1) + imm //addi
        }
        is(0x4.U) {
          reg(rd) := reg(rs1) ^ imm //xori
        }
        is(0x6.U) {
          reg(rd) := reg(rs1) | imm //ori
        }
        is(0x7.U) {
          reg(rd) := reg(rs1) & imm //andi
        }
        is(0x1.U) {
          reg(rd) := reg(rs1) << imm(4, 0) //slli
        }
        is(0x5.U) {
          switch(imm(11,5)) {
            is(0x00.U) {
              reg(rd) := reg(rs1) >> imm(4, 0) //srli
            }
            is(0x20.U) {                                      //***** mangler msb-extend
              reg(rd) := reg(rs1) >> imm(4, 0)
              when(reg(rs1)(31.U - imm(4,0))) {
                //val bal = 31
                reg(rd) := Cat(0xFFFFF.U, reg(rs1)(31,0)) //srai
              } otherwise {
                //reg(rd) := reg(rs1) >> imm(4, 0)
              }
            }
          }
        }
        is(0x2.U) {
          when(reg(rs1)(31) =/= imm(31)){ //slti
            reg(rd) := reg(rs1) > imm
          } otherwise {
            reg(rd) := reg(rs1) < imm
          }
        }
        is(0x3.U) {
          reg(rd) := reg(rs1) < imm //sltiu
        }
      }
    }
    is(0x03.U) { //I-type loads
      switch(funct3) {
        is(0x0.U) { //lb
          reg(rd) := mem.read(readAddr)
          when(mem.read(readAddr)(7)) { //sign-extend?
           reg(rd) := ~reg(rd)
          }
        }
        is(0x1.U) { //lh
          reg(rd) := Cat(0x0000.U,mem.read(readAddr + 1.U), mem.read(readAddr))
          when(mem.read(readAddr + 1.U)(7)) { //sign-extend
            reg(rd) := ~reg(rd)
          }
        }
        is(0x2.U) { //lw
          reg(rd) := Cat(mem.read(readAddr + 3.U), mem.read(readAddr + 2.U), mem.read(readAddr + 1.U), mem.read(readAddr))
        }
        is(0x4.U) { //lbu
          reg(rd) := mem.read(readAddr)
        }
        is(0x5.U) { //lhu
          reg(rd) := Cat(mem.read(readAddr + 1.U), mem.read(readAddr))
        }
      }
    }
    is(0x23.U) { //S-type
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
    is(0x63.U) { //B-type
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
  }

  // done should be set when the program ends, and the tester shall stop
  io.done := true.B

  // Make the register file visible to the tester
  //for (i <- 0 until 32) io.regDeb(i) := reg(i)
  io.pcDeb <> pc
  io.regDeb <> reg
  io.memDeb := Cat(mem.read(8.U), mem.read(9.U), mem.read(10.U), mem.read(11.U))
}