package day13

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class PacketScannersTest extends AnyFlatSpec {
  behavior of "severity"
  it should "handle the first example" in new SetupPuzzleData("example") {
    assert(PacketScanners.severity(PacketScanners.parse(lines)) == 24)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(PacketScanners.severity(PacketScanners.parse(lines)) == 2160)
  }
  behavior of "smallestDelayWithoutCatch"
  it should "handle the second example" in new SetupPuzzleData("example") {
    assert(PacketScanners.smallestDelayWithoutCatch(PacketScanners.parse(lines)) == 10)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(PacketScanners.smallestDelayWithoutCatch(PacketScanners.parse(lines)) == 3907470)
  }
}
