import chisel3.iotesters._
import org.scalatest._
import java.io._


class RiscVSpec extends FlatSpec with Matchers {
  "RiscV" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleCycleRiscV()) { c =>
      new PeekPokeTester(c) {

        //method for binary dump
        def writeFile(): Unit = {
          val outputStream = new FileOutputStream("binaryDump") //path to binary dump file
          for (i <- 0 until 32) {
            outputStream.write(peek(dut.io.regDeb0(i)).toByte)
            outputStream.write(peek(dut.io.regDeb1(i)).toByte)
            outputStream.write(peek(dut.io.regDeb2(i)).toByte)
            outputStream.write(peek(dut.io.regDeb3(i)).toByte)
          }
        }

        //method for comparing binary dump to .res file
        def compareFiles(): Unit = {
          val res = new FileInputStream("tests/additional/test_bgeu.res") //path to .res file
          val bin = new FileInputStream("binaryDump") //path to binary dump file
          var noError = true
          println("compare starts here:")
          for(i <- 0 until 128) {
            if(res.read() != bin.read()) {
              println("error: " + i)
              noError = false
            }
          }
          if(noError) println("Perfect match")
        }

        //method for printing register file to terminal
        def printReg() = {
          println("Register file (x0 .. x31)")
          for (i <- 0 until 32) {
            print(Integer.toHexString(peek(dut.io.regDeb(i)).toInt) + " ")
          }
          println()
        }

        //step through cycles until io.done is true
        do {
          step(1)
        }
        while(peek(dut.io.done) == 0)
        //print register file to terminal and "binaryDump" file
        printReg()
        writeFile()
        //compareFiles()
      }
    }
  }
}