package day11

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class HexEdTest extends AnyFlatSpec {
  behavior of "equivalentPath"
  it should "handle the examples" in {
    assert(HexEd.equivalentPath(HexEd.parsePath("ne,ne,ne")) == HexEd.parsePath("ne,ne,ne"))
    assert(HexEd.equivalentPath(HexEd.parsePath("ne,ne,sw,sw")) == HexEd.parsePath(""))
    assert(HexEd.equivalentPath(HexEd.parsePath("ne,ne,s,s")) == HexEd.parsePath("se,se"))
    assert(HexEd.equivalentPath(HexEd.parsePath("se,sw,se,sw,sw")).sorted == HexEd.parsePath("s,s,sw"))
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(HexEd.equivalentPath(HexEd.parsePath(lines.head)).length == 794)
  }
}
