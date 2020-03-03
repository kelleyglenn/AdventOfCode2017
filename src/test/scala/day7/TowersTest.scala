package day7

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class TowersTest extends AnyFlatSpec {
  behavior of "nameOfBottomProgram"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Towers.nameOfBottomProgram(lines) == "tknk")
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Towers.nameOfBottomProgram(lines) == "cyrupz")
  }
  behavior of "correctedWeightOfOffBalanceProgram"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(Towers.correctedWeightOfOffBalanceProgram(lines) == 60)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Towers.correctedWeightOfOffBalanceProgram(lines) == 193)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/" + getClass.getPackage.getName + "/" + name + ".txt"))
    val lines: List[String] = bufferedSource.getLines.toList
  }

}
