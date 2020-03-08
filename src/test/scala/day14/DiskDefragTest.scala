package day14

import org.scalatest.flatspec.AnyFlatSpec

class DiskDefragTest extends AnyFlatSpec {
  behavior of "hexToBinary"
  it should "handle the example" in {
    assert(DiskDefrag.hexToBinary("a0c2017") == "1010000011000010000000010111")
  }
  behavior of "squaresUsed"
  it should "handle the example" in {
    assert(DiskDefrag.squaresUsed("flqrgnkx") == 8108)
  }
  it should "solve the first puzzle" in {
    assert(DiskDefrag.squaresUsed("vbqugkhl") == 8148)
  }
  behavior of "usedSquaresToUsedNeighbors"
  it should "handle the example" in {
    val map = DiskDefrag.usedSquaresToUsedNeighbors("flqrgnkx")
    assert(map((0, 0)) == Set((0, 1)))
  }
  behavior of "regionCount"
  it should "handle the example" in {
    assert(DiskDefrag.regionCount("flqrgnkx") == 1242)
  }
  it should "solve the second puzzle" in {
    assert(DiskDefrag.regionCount("vbqugkhl") == 1180)
  }
}
