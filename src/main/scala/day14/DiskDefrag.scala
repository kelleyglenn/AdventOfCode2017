package day14

import day10.KnotHash

object DiskDefrag {
  def squaresUsed(key: String): Int = {
    val hashes: Seq[String] = (0 to 127).map(i => KnotHash.hash(key + "-" + Integer.toString(i)))
    val binaryStrings: Seq[String] = hashes.map(hexToBinary)
    binaryStrings.map(_.count(_ == '1')).sum
  }

  def hexToBinary(hash: String): String = {
    hash.toList.map { c: Char => ("0000" + Character.digit(c, 16).toBinaryString).takeRight(4) }.mkString
  }
}
