package day16

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class PermutationPromenadeTest extends AnyFlatSpec {
  behavior of "dance"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(PermutationPromenade.dance('a' to 'e', lines.head.split(',')).iterator.mkString == "baedc")
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(PermutationPromenade.dance('a' to 'p', lines.head.split(',')).iterator.mkString == "kpbodeajhlicngmf")
  }
}
