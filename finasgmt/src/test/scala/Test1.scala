import chisel3.iotesters.PeekPokeTester
import org.scalatest._

class Test1(dut: SingleCycleRiscV) extends PeekPokeTester(dut) {
  println("Test")
  step(10)
  dut.io.imem foreach(i => println(peek(i).toString()))
  true
}

class Test1Spec extends FlatSpec with Matchers {
  "RiscV " should "pass" in {
    chisel3.iotesters.Driver(() => new SingleCycleRiscV) { c => new Test1(c)} should be (true)
  }
}