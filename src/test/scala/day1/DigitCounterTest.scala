package day1

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class DigitCounterTest extends AnyFlatSpec {
  behavior of "sumRepeatsNext"
  it should "handle the first examples" in {
    assert(DigitCounter.sumRepeatsNext("1122") == 3)
    assert(DigitCounter.sumRepeatsNext("1111") == 4)
    assert(DigitCounter.sumRepeatsNext("1234") == 0)
    assert(DigitCounter.sumRepeatsNext("91212129") == 9)
  }
  it should "handle the second examples" in {
    assert(DigitCounter.sumRepeatsOpposite("1212") == 6)
    assert(DigitCounter.sumRepeatsOpposite("1221") == 0)
    assert(DigitCounter.sumRepeatsOpposite("123425") == 4)
    assert(DigitCounter.sumRepeatsOpposite("123123") == 12)
    assert(DigitCounter.sumRepeatsOpposite("12131415") == 4)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(DigitCounter.sumRepeatsNext(lines.head) == 1049)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(DigitCounter.sumRepeatsOpposite(lines.head) == 1508)
  }
}