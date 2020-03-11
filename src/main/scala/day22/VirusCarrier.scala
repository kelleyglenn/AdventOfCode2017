package day22

import scala.collection.mutable

object VirusCarrier {
  def countBurstsCausingInfection(startingMap: Seq[String], burstCount: Int): Int = {
    val grid: mutable.Map[Pos, Boolean] = parse(startingMap)
    var infectionCt = 0
    var curPos = Pos(0, 0)
    var curDirection = Pos(0, -1)

    def turn(right: Boolean): Unit = {
      val rightVal = if (right) 1 else -1
      curDirection = if (curDirection.x == 0) {
        Pos(if (curDirection.y == 1) -rightVal else rightVal, 0)
      } else {
        Pos(0, if (curDirection.x == 1) rightVal else -rightVal)
      }
    }

    (1 to burstCount).foreach { _ =>
      val curInfected = grid(curPos)
      turn(curInfected)
      if (!curInfected) infectionCt += 1
      grid(curPos) = !curInfected
      curPos = curPos + curDirection
    }
    infectionCt
  }

  private def parse(startingMap: Seq[String]): mutable.Map[Pos, Boolean] = {
    val middleY = (startingMap.size - 1) / 2
    val middleX = (startingMap.head.length - 1) / 2
    val map: mutable.Map[Pos, Boolean] = mutable.Map.empty.withDefaultValue(false)
    startingMap.indices.foreach { y =>
      startingMap(y).indices.foreach { x =>
        if (startingMap(y)(x) == '#') map.addOne(Pos(x - middleX, y - middleY) -> true)
      }
    }
    map
  }

  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = {
      Pos(x + other.x, y + other.y)
    }
  }

}
