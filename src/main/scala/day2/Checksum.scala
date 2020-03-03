package day2

object Checksum {
  def minFromMax(lines: Iterable[String]): Int = {
    lines.map(minFromMax).sum
  }

  def minFromMax(line: String): Int = {
    val ints: Array[Int] = line.split(" \t".toCharArray).map(_.toInt)
    ints.max - ints.min
  }

  def evenDivision(lines: Iterable[String]): Int = {
    lines.map(evenDivision).sum
  }

  def evenDivision(line: String): Int = {
    val ints: Set[Int] = line.split(" \t".toCharArray).map(_.toInt).toSet
    val evenDivisiblePair: (Int, Int) = ints.foldLeft(Set[(Int, Int)]())((pairs: Set[(Int, Int)], a: Int) => {
      var newPairs: Set[(Int, Int)] = Set[(Int, Int)]()
      ints.foreach((b: Int) => newPairs = newPairs + ((a, b)))
      pairs ++ newPairs
    }).filter((pair: (Int, Int)) => pair._1 != pair._2).find((pair: (Int, Int)) => pair._1 % pair._2 == 0).get
    evenDivisiblePair._1 / evenDivisiblePair._2
  }
}
