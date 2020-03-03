package day6

import org.scalatest.flatspec.AnyFlatSpec

class MemoryTest extends AnyFlatSpec {
  behavior of "reallocate"
  it should "handle the examples" in {
    assert(Memory.reallocate(List(0, 2, 7, 0)) == List(2, 4, 1, 2))
    assert(Memory.reallocate(List(2, 4, 1, 2)) == List(3, 1, 2, 3))
    assert(Memory.reallocate(List(3, 1, 2, 3)) == List(0, 2, 3, 4))
    assert(Memory.reallocate(List(0, 2, 3, 4)) == List(1, 3, 4, 1))
    assert(Memory.reallocate(List(1, 3, 4, 1)) == List(2, 4, 1, 2))
  }
  behavior of "cyclesUntilRepeat"
  it should "handle the example" in {
    assert(Memory.cyclesUntilRepeat(List(0, 2, 7, 0)) == 5)
  }
  it should "solve the first puzzle" in {
    assert(Memory.cyclesUntilRepeat(List(11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11)) == 4074)
  }
  behavior of "sizeOfLoop"
  it should "handle the example" in {
    assert(Memory.sizeOfLoop(List(0, 2, 7, 0)) == 4)
  }
  it should "solve the second puzzle" in {
    assert(Memory.sizeOfLoop(List(11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11)) == 2793)
  }
}
