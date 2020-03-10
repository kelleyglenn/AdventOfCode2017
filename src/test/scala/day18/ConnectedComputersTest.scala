package day18

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class ConnectedComputersTest extends AnyFlatSpec {
  behavior of "sentCount"
  it should "handle the example" in new SetupPuzzleData("example2") {
    assert(ConnectedComputers.sentCount(lines) == (3, 3))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(ConnectedComputers.sentCount(lines) == (7493, 7366))
  }
}
