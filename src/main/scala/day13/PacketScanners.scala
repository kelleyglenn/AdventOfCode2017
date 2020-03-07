package day13

object PacketScanners {
  def severity(scannerPositions: Seq[Int]): Int = {
    val depthsWhenCaught = scannerPositions.indices.map(i =>
      if (scannerPositionAtBeginningOfPsec(i, scannerPositions(i)) == 0) scannerPositions(i) else 0)
    depthsWhenCaught.zipWithIndex.map(e => e._1 * e._2).sum
  }

  private def scannerPositionAtBeginningOfPsec(psec: Int, positions: Int): Int = {
    val endIndex = positions - 1
    endIndex - Math.abs(psec % (endIndex * 2) - endIndex)
  }

  def smallestDelayWithoutCatch(scannerPositions: Seq[Int]): Int = {
    var delay = 0
    while (caught(scannerPositions, delay)) delay += 1
    delay
  }

  def caught(scannerPositions: Seq[Int], delay: Int): Boolean = {
    scannerPositions.indices.exists(i => scannerPositionAtBeginningOfPsec(i + delay, scannerPositions(i)) == 0)
  }

  def parse(lines: Iterable[String]): Seq[Int] = {
    val indexPositionsMap: Map[Int, Int] =
      lines.map(s => s.split(": ")).map(sa => (sa(0).toInt, sa(1).toInt)).toMap.withDefaultValue(0)
    (0 to indexPositionsMap.keys.max).map(i => indexPositionsMap(i))
  }
}
