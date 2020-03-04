package day9

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class StreamProcessingTest extends AnyFlatSpec {
  behavior of "scoreWithGarbageCount"
  it should "handle the first examples" in {
    assert(StreamProcessing.scoreWithGarbageCount("{}")._1 == 1)
    assert(StreamProcessing.scoreWithGarbageCount("{{{}}}")._1 == 6)
    assert(StreamProcessing.scoreWithGarbageCount("{{},{}}")._1 == 5)
    assert(StreamProcessing.scoreWithGarbageCount("{{{},{},{{}}}}")._1 == 16)
    assert(StreamProcessing.scoreWithGarbageCount("{<a>,<a>,<a>,<a>}")._1 == 1)
    assert(StreamProcessing.scoreWithGarbageCount("{{<ab>},{<ab>},{<ab>},{<ab>}}")._1 == 9)
    assert(StreamProcessing.scoreWithGarbageCount("{{<!!>},{<!!>},{<!!>},{<!!>}}")._1 == 9)
    assert(StreamProcessing.scoreWithGarbageCount("{{<a!>},{<a!>},{<a!>},{<ab>}}")._1 == 3)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(StreamProcessing.scoreWithGarbageCount(lines.head)._1 == 13154)
  }
  it should "handle the second examples" in {
    assert(StreamProcessing.scoreWithGarbageCount("{<>}")._2 == 0)
    assert(StreamProcessing.scoreWithGarbageCount("{<random characters>}")._2 == 17)
    assert(StreamProcessing.scoreWithGarbageCount("{<<<<>}")._2 == 3)
    assert(StreamProcessing.scoreWithGarbageCount("{<{!>}>}")._2 == 2)
    assert(StreamProcessing.scoreWithGarbageCount("{<!!>}")._2 == 0)
    assert(StreamProcessing.scoreWithGarbageCount("{<!!!>>}")._2 == 0)
    assert(StreamProcessing.scoreWithGarbageCount("{<{o\"i!a,<{i<a>}")._2 == 10)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(StreamProcessing.scoreWithGarbageCount(lines.head)._2 == 6369)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/" + getClass.getPackage.getName + "/" + name + ".txt"))
    val lines: List[String] = bufferedSource.getLines.toList
  }

}
