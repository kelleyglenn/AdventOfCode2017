package util

import scala.io.{BufferedSource, Source}

class SetupPuzzleData(name: String) {
  val bufferedSource: BufferedSource =
    Source.fromURL(getClass.getResource("/" + getClass.getPackage.getName + "/" + name + ".txt"))
  val lines: List[String] = bufferedSource.getLines.toList
}