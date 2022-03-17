package day23

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class CoprocessorTest extends AnyFlatSpec {
  behavior of "timesMulInvoked"
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Coprocessor.timesMulInvoked(lines) == 3969)
  }
  behavior of "valueOfHWithDebugFlag"
  it should "handle no debug flag" in new SetupPuzzleData("input") {
    assert(Coprocessor.valueOfHWithDebugFlag(lines, setDebugFlag = false) == 1)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input-optimized") {
    // Program exits when b == c at line 27
    // c = 123500
    // b starts at 106500 and h starts at 1
    // b increases by 17*2 when h goes up by 1
    // starting c - starting b = 17000, so h will increase by 500
    // 501 is too low
    assert(Coprocessor.valueOfHWithDebugFlag(lines) == 3969)
  }
}
