package day12

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class PlumberTest extends AnyFlatSpec {
  behavior of "reachableFrom0Count"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Plumber.reachableFrom0Count(Plumber.parse(lines)) == 6)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Plumber.reachableFrom0Count(Plumber.parse(lines)) == 134)
  }
  behavior of "groups"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Plumber.groups(Plumber.parse(lines)) == Set(Set(0, 2, 3, 4, 5, 6), Set(1)))
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Plumber.groups(Plumber.parse(lines)).size == 193)
  }
}
