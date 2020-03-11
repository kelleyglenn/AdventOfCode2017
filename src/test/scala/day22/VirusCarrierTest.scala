package day22

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class VirusCarrierTest extends AnyFlatSpec {
  behavior of "countBurstsCausingInfection"
  it should "handle the example" in {
    val map = List("..#", "#..", "...")
    assert(VirusCarrier.countBurstsCausingInfection(map, 7) == 5)
    assert(VirusCarrier.countBurstsCausingInfection(map, 70) == 41)
    assert(VirusCarrier.countBurstsCausingInfection(map, 10000) == 5587)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(VirusCarrier.countBurstsCausingInfection(lines, 10000) == 5450)
  }
}
