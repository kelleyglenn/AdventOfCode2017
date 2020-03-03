package day5

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class JumpsTest extends AnyFlatSpec {
  behavior of "countJumpsUntilExit"
  it should "handle the first example" in {
    assert(Jumps.countJumpsUntilExit(Array(0, 3, 0, 1, -3), _ => 1) == 5)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(Jumps.countJumpsUntilExit(lines.map(_.toInt).toArray, _ => 1) == 359348)
  }
  it should "handle the second example" in {
    assert(Jumps.countJumpsUntilExit(Array(0, 3, 0, 1, -3), i => if (i >= 3) -1 else 1) == 10)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(Jumps.countJumpsUntilExit(lines.map(_.toInt).toArray, i => if (i >= 3) -1 else 1) == 27688760)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/" + getClass.getPackage.getName + "/" + name + ".txt"))
    val lines: List[String] = bufferedSource.getLines.toList
  }

}
