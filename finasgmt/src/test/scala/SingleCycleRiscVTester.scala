import chisel3.iotesters._
import org.scalatest._

class RiscVSpec extends FlatSpec with Matchers {
  "RiscV" should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleCycleRiscV()) { c =>
      new PeekPokeTester(c) {

        def printReg() = {
          for (i <- 0 until 32) {
            print("x" + i + "\t")
          }
          println()
          for (i <- 0 until 32) {
            print(Integer.toHexString(peek(dut.io.regDeb(i)).toInt) + "\t")
          }
          println()
        }

        for (i <- 0 until 6) {
          step(1)
          println(peek(dut.io.pcDeb) + ": 0x" + Integer.toHexString(peek(dut.io.imemDeb(i)).toInt))
          printReg()
        }
      }
    }
  }
}