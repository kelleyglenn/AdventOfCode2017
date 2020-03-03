package day3

object SpiralMemory {

  def distanceToAccessPort(location: Int): Int = {
    val locationCoords: (Int, Int) = coordsOf(location)
    Math.abs(locationCoords._1) + Math.abs(locationCoords._2)
  }

  def coordsOf(location: Int): (Int, Int) = {
    val smallestOddSquareGElocation = smallestOddSquareGE(location)
    val spiralWidth = Math.sqrt(smallestOddSquareGElocation).toInt - 1
    val spiralDist = spiralWidth / 2
    if (location >= smallestOddSquareGElocation - spiralWidth)
      (location - (smallestOddSquareGElocation - spiralDist), -spiralDist)
    else if (location >= smallestOddSquareGElocation - spiralWidth * 2)
      (-spiralDist, (smallestOddSquareGElocation - spiralDist * 3) - location)
    else if (location >= smallestOddSquareGElocation - spiralWidth * 3)
      (location - (smallestOddSquareGElocation - spiralDist * 5), spiralDist)
    else
      (spiralDist, location - (smallestOddSquareGElocation - spiralDist * 7))
  }

  private def smallestOddSquareGE(location: Int): Int = {
    var factor = 1
    while (factor * factor < location) factor += 2
    factor * factor
  }

  def nextValueLargerThan(target: Int): Int = {
    var curCoords = (0, 0)
    var curSpiralDist = 0
    var knownValues: Map[(Int, Int), Int] = Map(curCoords -> 1)

    def next: (Int, Int) = {
      if (curCoords._1 == curSpiralDist) {
        if (curCoords._2 == -curSpiralDist) {
          curSpiralDist += 1
          (curSpiralDist, 1 - curSpiralDist)
        }
        else if (curCoords._2 < curSpiralDist) (curCoords._1, curCoords._2 + 1)
        else (curCoords._1 - 1, curCoords._2)
      }
      else if (curCoords._2 == curSpiralDist) {
        if (curCoords._1 > -curSpiralDist) (curCoords._1 - 1, curCoords._2)
        else (curCoords._1, curCoords._2 - 1)
      }
      else if (curCoords._1 == -curSpiralDist) {
        if (curCoords._2 > -curSpiralDist) (curCoords._1, curCoords._2 - 1)
        else (curCoords._1 + 1, curCoords._2)
      }
      else (curCoords._1 + 1, curCoords._2)
    }

    while (knownValues(curCoords) <= target) {
      curCoords = next
      knownValues = knownValues + (curCoords -> sumOfNeighbors(knownValues, curCoords))
    }
    knownValues(curCoords)
  }

  def sumOfNeighbors(knownValues: Map[(Int, Int), Int], curCoords: (Int, Int)): Int = {
    val knownValuesWithDefault = knownValues.withDefaultValue(0)
    (-1 to 1).map { x =>
      (-1 to 1).map(y => knownValuesWithDefault((curCoords._1 + x, curCoords._2 + y))).sum
    }.sum
  }
}
