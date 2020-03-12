package day22

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class VirusCarrierTest extends AnyFlatSpec {
  behavior of "countBurstsCausingInfection"
  it should "handle the first example" in {
    val map = List("..#", "#..", "...")
    assert(VirusCarrier.countBurstsCausingInfection(map, 7, VirusCarrier.Part1Ops) == 5)
    assert(VirusCarrier.countBurstsCausingInfection(map, 70, VirusCarrier.Part1Ops) == 41)
    assert(VirusCarrier.countBurstsCausingInfection(map, 10000, VirusCarrier.Part1Ops) == 5587)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(VirusCarrier.countBurstsCausingInfection(lines, 10000, VirusCarrier.Part1Ops) == 5450)
  }
  it should "handle the second example" in {
    val map = List("..#", "#..", "...")
    assert(VirusCarrier.countBurstsCausingInfection(map, 100, VirusCarrier.Part2Ops) == 26)
    assert(VirusCarrier.countBurstsCausingInfection(map, 10000000, VirusCarrier.Part2Ops) == 2511944)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(VirusCarrier.countBurstsCausingInfection(lines, 10000000, VirusCarrier.Part2Ops) == 2511957)
  }
}
