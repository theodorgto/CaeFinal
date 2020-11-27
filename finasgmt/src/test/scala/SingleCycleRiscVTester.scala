import chisel3.iotesters._
import org.scalatest._
import scala.util.control.Breaks._

class RiscVSpec extends FlatSpec with Matchers {
  "RiscV" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleCycleRiscV()) { c =>
      new PeekPokeTester(c) {

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
        //1245 cycles to finish L7
      }
    }
  }
}