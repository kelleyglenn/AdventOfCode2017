package day14

import day10.KnotHash
import day12.Plumber

object DiskDefrag {
  def squaresUsed(key: String): Int = {
    val binaryStrings: scala.Seq[String] = keyToBinaryString(key)
    binaryStrings.map(_.count(_ == '1')).sum
  }

  def regionCount(key: String): Int = {
    Plumber.groups(usedSquaresToUsedNeighbors(key)).size
  }

  def hexToBinary(hash: String): String = {
    hash.toList.map { c: Char => ("0000" + Character.digit(c, 16).toBinaryString).takeRight(4) }.mkString
  }

  def usedSquaresToUsedNeighbors(key: String): Map[(Int, Int), Set[(Int, Int)]] = {
    val usedSquares: Seq[Seq[Boolean]] = keyToBinaryString(key).map(_.map(_ == '1'))
    usedSquares.indices.foldLeft(Map[(Int, Int), Set[(Int, Int)]]()) {
      case (map, y) => usedSquares(y).indices.foldLeft(map) {
        case (map, x) => if (usedSquares(y)(x)) map + ((y, x) -> Set((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)).filter {
          case (y, x) => usedSquares.indices.contains(y) && usedSquares(y).indices.contains(x) && usedSquares(y)(x)
        }) else map
      }
    }
  }

  private def keyToBinaryString(key: String): Seq[String] = {
    (0 to 127).map(i => KnotHash.hash(key + "-" + Integer.toString(i))).map(hexToBinary)
  }
}
