package day22

import scala.collection.mutable

object VirusCarrier {
  def countBurstsCausingInfection[A](startingMap: Seq[String], burstCount: Int, nodeInfo: NodeStateOps[A]): Int = {
    val grid: mutable.Map[Pos, A] = parse(startingMap, nodeInfo)
    var infectionCt = 0
    var curPos: Pos = Pos(0, 0)
    var curDirection: Pos = Pos(0, -1)
    (1 to burstCount).foreach { _: Int =>
      var curState: A = grid(curPos)
      curDirection = nodeInfo.turn(curDirection, curState)
      curState = nodeInfo.nextState(curState)
      grid(curPos) = curState
      if (curState == nodeInfo.infectedState) infectionCt += 1
      curPos = curPos + curDirection
    }
    infectionCt
  }

  private def parse[B](startingMap: Seq[String], nodeInfo: NodeStateOps[B]): mutable.Map[Pos, B] = {
    val middleY: Int = (startingMap.size - 1) / 2
    val middleX: Int = (startingMap.head.length - 1) / 2
    val map: mutable.Map[Pos, B] = mutable.Map.empty.withDefaultValue(nodeInfo.cleanState)
    startingMap.indices.foreach { y: Int =>
      startingMap(y).indices.foreach { x: Int =>
        if (startingMap(y)(x) == '#') map.addOne(Pos(x - middleX, y - middleY) -> nodeInfo.infectedState)
      }
    }
    map
  }

  trait NodeStateOps[NS] {
    val infectedState: NS
    val cleanState: NS

    val nextState: Map[NS, NS]

    def turn(curDirection: Pos, curState: NS): Pos
  }

  object Part1Ops extends NodeStateOps[Boolean] {
    override val cleanState = false
    override val infectedState = true
    override val nextState: Map[Boolean, Boolean] =
      Map(cleanState -> infectedState, infectedState -> cleanState)

    override def turn(curDirection: Pos, curState: Boolean): Pos = {
      val rightVal: Int = if (curState == infectedState) 1 else -1
      if (curDirection.x == 0) Pos(if (curDirection.y == 1) -rightVal else rightVal, 0)
      else Pos(0, if (curDirection.x == 1) rightVal else -rightVal)
    }
  }

  object Part2States extends Enumeration {
    val Clean, Weakened, Infected, Flagged = Value
  }

  object Part2Ops extends NodeStateOps[Part2States.Value] {
    override val cleanState: Part2States.Value = Part2States.Clean
    override val infectedState: Part2States.Value = Part2States.Infected
    override val nextState: Map[Part2States.Value, Part2States.Value] =
      Map(Part2States.Clean -> Part2States.Weakened, Part2States.Weakened -> Part2States.Infected,
        Part2States.Infected -> Part2States.Flagged, Part2States.Flagged -> Part2States.Clean)

    override def turn(curDirection: Pos, curState: Part2States.Value): Pos = {
      if (curState == Part2States.Infected || curState == Part2States.Clean)
        Part1Ops.turn(curDirection, curState == Part2States.Infected)
      else if (curState == Part2States.Flagged) Pos(-curDirection.x, -curDirection.y)
      else curDirection
    }
  }

  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = {
      Pos(x + other.x, y + other.y)
    }
  }

}
