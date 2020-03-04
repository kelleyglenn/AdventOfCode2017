package day5

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class JumpsTest extends AnyFlatSpec {
  behavior of "countJumpsUntilExit"
  it should "handle the first example" in {
    assert(Jumps.countJumpsUntilExit(Array(0, 3, 0, 1, -3), (_: Int) => 1) == 5)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Jumps.countJumpsUntilExit(lines.map(_.toInt).toArray, (_: Int) => 1) == 359348)
  }
  it should "handle the second example" in {
    assert(Jumps.countJumpsUntilExit(Array(0, 3, 0, 1, -3), (i: Int) => if (i >= 3) -1 else 1) == 10)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Jumps.countJumpsUntilExit(lines.map(_.toInt).toArray, (i: Int) => if (i >= 3) -1 else 1) == 27688760)
  }
}
