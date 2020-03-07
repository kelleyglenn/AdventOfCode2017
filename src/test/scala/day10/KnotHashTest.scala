package day10

import org.scalatest.flatspec.AnyFlatSpec

class KnotHashTest extends AnyFlatSpec {
  behavior of "firstTwoNumbersAfterSwaps"
  it should "handle the first example" in {
    assert(KnotHash.firstTwoNumbersAfterSwaps(5, List(3, 4, 1, 5)) == (3, 4))
  }
  it should "solve the first puzzle" in {
    val firstTwo: (Int, Int) = KnotHash.firstTwoNumbersAfterSwaps(256, List(102, 255, 99, 252, 200, 24, 219, 57, 103, 2, 226, 254, 1, 0, 69, 216))
    assert(firstTwo == (39, 143))
    assert(firstTwo._1 * firstTwo._2 == 5577)
  }
  behavior of "xor"
  it should "handle the example" in {
    assert(KnotHash.xor(List(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)) == 64)
  }
  behavior of "hexString"
  it should "handle the example" in {
    assert(KnotHash.hexString(List(64, 7, 255)) == "4007ff")
  }
  behavior of "hash"
  it should "handle the second set of examples" in {
    assert(KnotHash.hash("") == "a2582a3a0e66e6e86e3812dcb672a272")
    assert(KnotHash.hash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd")
    assert(KnotHash.hash("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d")
    assert(KnotHash.hash("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e")
  }
  it should "solve the second puzzle" in {
    assert(KnotHash.hash("102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216") == "44f4befb0f303c0bafd085f97741d51d")
  }
}
