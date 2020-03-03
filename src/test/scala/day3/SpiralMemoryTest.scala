package day3

import org.scalatest.flatspec.AnyFlatSpec

class SpiralMemoryTest extends AnyFlatSpec {
  behavior of "coordsOf"
  it should "handle the examples" in {
    assert(SpiralMemory.coordsOf(1) == (0, 0))
    assert(SpiralMemory.coordsOf(12) == (2, 1))
    assert(SpiralMemory.coordsOf(23) == (0, -2))
  }

  behavior of "distanceToAccessPort"
  it should "handle the examples" in {
    assert(SpiralMemory.distanceToAccessPort(1) == 0)
    assert(SpiralMemory.distanceToAccessPort(12) == 3)
    assert(SpiralMemory.distanceToAccessPort(23) == 2)
    assert(SpiralMemory.distanceToAccessPort(1024) == 31)
  }
  it should "solve the first puzzle" in {
    assert(SpiralMemory.distanceToAccessPort(325489) == 552)
  }

  behavior of "nextValueLargerThan"
  it should "handle the examples" in {
    assert(SpiralMemory.nextValueLargerThan(1) == 2)
    assert(SpiralMemory.nextValueLargerThan(5) == 10)
    assert(SpiralMemory.nextValueLargerThan(147) == 304)
    assert(SpiralMemory.nextValueLargerThan(806) == 880)
  }
  it should "solve the second puzzle" in {
    assert(SpiralMemory.nextValueLargerThan(325489) == 330785)
  }
}
