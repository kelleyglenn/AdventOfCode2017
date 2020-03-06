package day11

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class HexEdTest extends AnyFlatSpec {
  behavior of "maxAndFinalDistance"
  it should "handle the examples" in {
    assert(HexEd.maxAndFinalDistance(HexEd.parsePath("ne,ne,ne")) == (3, 3))
    assert(HexEd.maxAndFinalDistance(HexEd.parsePath("ne,ne,sw,sw")) == (2, 0))
    assert(HexEd.maxAndFinalDistance(HexEd.parsePath("ne,ne,s,s")) == (2, 2))
    assert(HexEd.maxAndFinalDistance(HexEd.parsePath("se,sw,se,sw,sw")) == (3, 3))
  }
  it should "solve the first and second puzzle" in new SetupPuzzleData("input") {
    assert(HexEd.maxAndFinalDistance(HexEd.parsePath(lines.head)) == (1524, 794))
  }
}
