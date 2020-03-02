package day2

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class ChecksumTest extends AnyFlatSpec {
  behavior of "minFromMax String"
  it should "handle my tests" in {
    assert(Checksum.minFromMax("9 1") == 8)
    assert(Checksum.minFromMax("9\t1") == 8)
    assert(Checksum.minFromMax("2 6 1 9 8 4 7") == 8)
  }

  behavior of "minFromMax Strings"
  it should "handle the example" in {
    val strings: List[String] = "5 1 9 5\n7 5 3\n2 4 6 8".split('\n').toList
    assert(Checksum.minFromMax(strings) == 18)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Checksum.minFromMax(lines) == 32121)
  }

  behavior of "evenDivision String"
  it should "handle my tests" in {
    assert(Checksum.evenDivision("9 1") == 9)
    assert(Checksum.evenDivision("9\t1") == 9)
    assert(Checksum.evenDivision("6 9 8 4 7") == 2)
  }

  behavior of "evenDivision Strings"
  it should "handle the example" in {
    val strings: List[String] = "5 9 2 8\n9 4 7 3\n3 8 6 5".split('\n').toList
    assert(Checksum.evenDivision(strings) == 9)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Checksum.evenDivision(lines) == 197)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/" + getClass.getPackage.getName + "/" + name + ".txt"))
    val lines: List[String] = bufferedSource.getLines.toList
  }

}
