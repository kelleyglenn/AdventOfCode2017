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
  behavior of "danceXslow"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(PermutationPromenade.danceXslow('a' to 'e', lines.head.split(','), 2).iterator.mkString == "ceadb")
  }
  behavior of "danceX"
  it should "provide the same results as danceXslow" in new SetupPuzzleData("example") {
    assert(PermutationPromenade.danceX('a' to 'e', lines.head.split(','), 2).iterator.mkString ==
      PermutationPromenade.danceXslow('a' to 'e', lines.head.split(','), 2).iterator.mkString)
    assert(PermutationPromenade.danceX('a' to 'e', lines.head.split(','), 200).iterator.mkString ==
      PermutationPromenade.danceXslow('a' to 'e', lines.head.split(','), 200).iterator.mkString)
    assert(PermutationPromenade.danceX('a' to 'e', lines.head.split(','), 201).iterator.mkString ==
      PermutationPromenade.danceXslow('a' to 'e', lines.head.split(','), 201).iterator.mkString)
  }
  it should "demonstrate puzzle performance 1K" in new SetupPuzzleData("input") {
    assert(PermutationPromenade.danceX('a' to 'p', lines.head.split(','), 1000).iterator.mkString.length == 16)
  }
  it should "demonstrate puzzle performance 1M" in new SetupPuzzleData("input") {
    assert(PermutationPromenade.danceX('a' to 'p', lines.head.split(','), 1000000).iterator.mkString.length == 16)
  }
  it should "demonstrate puzzle performance 1G" in new SetupPuzzleData("input") {
    assert(PermutationPromenade.danceX('a' to 'p', lines.head.split(','), 1000000000).iterator.mkString.length == 16)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(PermutationPromenade.danceX('a' to 'p', lines.head.split(','), 1000000000).iterator.mkString == "ahgpjdkcbfmneloi")
  }
}
