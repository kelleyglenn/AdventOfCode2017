package day11

import day11.HexEd.Direction.Direction

object HexEd {
  def parsePath(path: String): IterableOnce[Direction] = {
    if (path.isEmpty) Seq.empty
    else path.split(',').map((s: String) => Direction.withName(s.toUpperCase))
  }

  def maxAndFinalDistance(path: IterableOnce[Direction]): (Int, Int) = {
    var maxDistance: Int = 0
    var position: (Int, Int, Int) = (0, 0, 0) //nw,n,ne
    path.iterator.foreach((d: Direction) => {
      position = standardizePos(position._1 + d.nwDiff, position._2 + d.nDiff, position._3 + d.neDiff)
      maxDistance = Math.max(maxDistance, distance(position))
    })
    (maxDistance, distance(position))
  }

  private def distance(position: (Int, Int, Int)): Int = {
    Math.abs(position._1) + Math.abs(position._2) + Math.abs(position._3)
  }

  private def standardizePos(pos: (Int, Int, Int)): (Int, Int, Int) = {
    var newPos: (Int, Int, Int) = pos
    if (newPos._1 < 0 && newPos._2 > 0) newPos = (newPos._1 + 1, newPos._2 - 1, newPos._3 + 1)
    if (newPos._1 > 0 && newPos._2 < 0) newPos = (newPos._1 - 1, newPos._2 + 1, newPos._3 + 1)
    if (newPos._3 < 0 && newPos._2 > 0) newPos = (newPos._1 + 1, newPos._2 - 1, newPos._3 + 1)
    if (newPos._3 > 0 && newPos._2 < 0) newPos = (newPos._1 - 1, newPos._2 + 1, newPos._3 - 1)
    if (newPos._1 > 0 && newPos._3 > 0) newPos = (newPos._1 - 1, newPos._2 + 1, newPos._3 - 1)
    if (newPos._1 < 0 && newPos._3 < 0) newPos = (newPos._1 + 1, newPos._2 - 1, newPos._3 + 1)
    newPos
  }

  object Direction extends Enumeration {
    type Direction = Value

    val NW: HexDiffs = HexDiffs(1, 0, 0)
    val SE: HexDiffs = HexDiffs(-1, 0, 0)
    val N: HexDiffs = HexDiffs(0, 1, 0)
    val S: HexDiffs = HexDiffs(0, -1, 0)
    val NE: HexDiffs = HexDiffs(0, 0, 1)
    val SW: HexDiffs = HexDiffs(0, 0, -1)

    import scala.language.implicitConversions

    implicit def valueToDirectionVal(x: Value): HexDiffs = x.asInstanceOf[HexDiffs]

    protected case class HexDiffs(nwDiff: Int, nDiff: Int, neDiff: Int) extends super.Val

  }

}
