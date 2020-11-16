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
    val imemDeb = Output(Vec(32, SInt(32.W)))
  })

  for (i <- 0 until 32) io.imemDeb(i) := 0.S
  val program = CopyBytes("tests/task1/addneg.bin") //load machine code from file
  val imem = VecInit(program.map(_.S(32.W)))  //map to chisel vec
  for (i <- program.indices) io.imemDeb(i) := imem(i) //for the tester


  val pc = RegInit(0.U(32.W))

  // TODO: there should be an elegant way to express this
  val vec = Wire(Vec(32, UInt(32.W)))
  for (i <- 0 until 32) vec(i) := 0.U
  // We initialize the register file to 0 for a nicer display
  // In a real processor this is usually not done
  val reg = RegInit(vec)

  val instr = imem(pc(31, 2))

  val opcode = instr(6, 0)
  val rd = instr(11, 7)
  val rs1 = instr(19, 15)
  val rs2 = instr(24, 20)
  val funct3 = instr(14, 12)
  val funct7 = instr(31 ,25)
  val imm = WireInit(0.U (32.W))
  var sign = instr(31)
  when (sign) {                           //sign extension
    imm := Cat(0xFFFFF.U, instr(31, 20))
  } otherwise {
    imm := instr(31, 20)
  }
  val shift = WireInit(0.U (5.W)) //kan måske ændres til reg(rs2) direkte

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
        /*
        is(0x5.U) {
          switch(funct7) {
            is(0x00.U) {
              reg(rd) := reg(rs1) >> reg(rs2) //srl
            }
            is(0x20.U) {
              reg(rd) := reg(rs1) >> reg(rs2) //sra ***** mangler msb-extend
            }
          }
        }

         */
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
        //is(0x1.U) {
          //reg(rd) := reg(rs1) << imm(0, 4) //slli
        //}
        /*
        is(0x5.U) {
          switch(imm(5, 11)) {
            is(0x00.U) {
              reg(rd) := reg(rs1) >> imm(0, 4) //srli
            }
            is(0x20.U) {
              reg(rd) := reg(rs1) >> imm(0, 4) //srai ***** mangler msb-extend
            }
          }
        }*/
        is(0x2.U) {
          reg(rd) := reg(rs1) < imm //slti ****** virker måske ikke?
        }
        is(0x3.U) {
          reg(rd) := reg(rs1) < imm //sltiu ****** virker måske ikke? mangler zero-extend
        }
      }
    }
    is(0x03.U) {
      switch(funct3) {
        is(0x0.U) {
          reg(rd) := reg(rs1) + imm //lb ****** implement memory
        }
      }
      /*
      is(0x23.U) { //S-type
      }

       */
    }
  }

  pc := pc + 4.U

  // done should be set when the program ends, and the tester shall stop
  io.done := true.B

  // Make the register file visible to the tester
  //for (i <- 0 until 32) io.regDeb(i) := reg(i)
  io.regDeb <> reg
}


