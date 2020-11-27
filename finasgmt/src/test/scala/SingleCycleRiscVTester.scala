import chisel3.iotesters._
import org.scalatest._
import scala.util.control.Breaks._
import java.io._


class RiscVSpec extends FlatSpec with Matchers {
  "RiscV" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleCycleRiscV()) { c =>
      new PeekPokeTester(c) {

        def writeFile(): Unit = {
          //val inputStream = new BufferedInputStream(new FileInputStream(""))
          val outputStream = new FileOutputStream("binaryDump")
          for (i <- 0 until 32) {
            outputStream.write(peek(dut.io.regDeb0(i)).toByte)
            outputStream.write(peek(dut.io.regDeb1(i)).toByte)
            outputStream.write(peek(dut.io.regDeb2(i)).toByte)
            outputStream.write(peek(dut.io.regDeb3(i)).toByte)
          }
        }

        def compareFiles(): Unit = {
          val bin = new FileInputStream("tests/task3/loop.res")
          val res = new FileInputStream("binaryDump")
          var noError = true
          println("compare starts here:")
          for(i <- 0 until 128) {
            if(bin.read() != res.read()) {
              println("error: " + i)
              noError = false
            }
          }
          if(noError) println("Perfect match")
        }

        def printReg() = {
          //for (i <- 0 until 32) {
          //  print("x" + i + "\t")
          //}
          //println()
          for (i <- 0 until 32) {
            print("x" + i + ": " + Integer.toHexString(peek(dut.io.regDeb(i)).toInt) + "\t")
          }
          println()
        }

        var i = 0

        do {
          //if (i > 1245) println("PC = " + peek(dut.io.pcDeb) + ": 0x" + Integer.toHexString(peek(dut.io.imemDeb(peek(dut.io.pcDeb).toInt / 4)).toInt))
          step(1)
          i += 1
          //if (i > 1245) printReg()
        }
        while(peek(dut.io.done) == 0 && i < 3000)
        printReg()
        writeFile()
        compareFiles()
        //1245 cycles to finish L7
      }
    }
  }
}