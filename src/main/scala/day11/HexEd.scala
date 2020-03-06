package day11

import day11.HexEd.Direction.{Direction, N, NE, NW, S, SE, SW}

object HexEd {
  val destructors: Set[(Direction, Direction)] = Set((N, S), (NW, SE), (NE, SW))
  val reducers: Map[(Direction, Direction), Direction] = Map(
    (N, SW) -> NW, (N, SE) -> NE, (S, NW) -> SW,
    (S, NE) -> SE, (NE, NW) -> N, (SE, SW) -> S)

  def equivalentPath(path: Seq[Direction]): Seq[Direction] = {
    var newPath: Seq[Direction] = path
    while (destructible(newPath) || reducible(newPath)) {
      while (destructible(newPath)) newPath = destroy(newPath)
      while (reducible(newPath)) newPath = reduce(newPath)
    }
    newPath
  }

  private def destructible(path: Seq[Direction]): Boolean = {
    destructors.exists { case (d1, d2) => path.contains(d1) && path.contains(d2) }
  }

  private def reducible(path: Seq[Direction]): Boolean = {
    reducers.keySet.exists { case (d1, d2) => path.contains(d1) && path.contains(d2) }
  }

  private def destroy(path: Seq[Direction]): Seq[Direction] = {
    destructors.filter { case (d1, d2) => path.contains(d1) && path.contains(d2) }.head match {
      case (d1, d2) => removeFirst(removeFirst(path, d1), d2)
    }
  }

  private def reduce(path: Seq[Direction]): Seq[Direction] = {
    reducers.keySet.filter { case (d1, d2) => path.contains(d1) && path.contains(d2) }.head match {
      case (d1, d2) => removeFirst(removeFirst(path, d1), d2).appended(reducers((d1, d2)))
    }
  }

  private def removeFirst(path: Seq[Direction], d: Direction): Seq[Direction] = {
    val index: Int = path.indexOf(d)
    path.take(index) ++ path.takeRight(path.length - index - 1)
  }

  def parsePath(path: String): Seq[Direction] = {
    if (path.isEmpty) Seq.empty
    else path.split(',').map((s: String) => Direction.withName(s.toUpperCase))
  }

  object Direction extends Enumeration {
    type Direction = Value
    val N, S, NE, NW, SE, SW = Value
  }

}
