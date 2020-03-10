package day19

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class TubesTest extends AnyFlatSpec {
  behavior of "lettersEncounteredAndSteps"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Tubes.lettersEncounteredAndSteps(lines.toVector) == ("ABCDEF", 38))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(Tubes.lettersEncounteredAndSteps(lines.toVector) == ("PBAZYFMHT", 16072))
  }
}
