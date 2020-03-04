package day8

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class RegisterInstructionsTest extends AnyFlatSpec {
  behavior of "run"
  it should "handle the first part example" in new SetupPuzzleData("example") {
    val ri = new RegisterInstructions(Instruction.parse(lines))
    val reg: Map[String, Int] = ri.run()._1
    assert(reg.values.max == 1)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    val ri = new RegisterInstructions(Instruction.parse(lines))
    val reg: Map[String, Int] = ri.run()._1
    assert(reg.values.max == 6343)
  }
  it should "handle the second part example" in new SetupPuzzleData("example") {
    val ri = new RegisterInstructions(Instruction.parse(lines))
    val highest: Int = ri.run()._2
    assert(highest == 10)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    val ri = new RegisterInstructions(Instruction.parse(lines))
    val highest: Int = ri.run()._2
    assert(highest == 7184)
  }
}
