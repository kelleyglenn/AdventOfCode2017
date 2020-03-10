package day18

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class DuetTest extends AnyFlatSpec {
  behavior of "firstRecoveredFrequency"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Duet.firstRecoveredFrequency(lines).contains(4))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Duet.firstRecoveredFrequency(lines).contains(2951))
  }
}