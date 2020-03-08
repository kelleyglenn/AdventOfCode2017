package day12

object Plumber {
  def reachableFrom0Count(neighbors: Map[Int, Set[Int]]): Int = {
    reachableFrom(0, neighbors).size
  }

  private def reachableFrom[A](from: A, neighbors: Map[A, Set[A]]): Set[A] = {
    var visited: Set[A] = Set(from)

    def visitedWithUnvisitedNeighbors: Option[A] = {
      visited.find(i => neighbors(i).exists(n => !visited.contains(n)))
    }

    while (visitedWithUnvisitedNeighbors.nonEmpty) {
      visited = visited ++ neighbors(visitedWithUnvisitedNeighbors.get)
    }
    visited
  }

  def groups[A](neighbors: Map[A, Set[A]]): Set[Set[A]] = {
    var groups: Set[Set[A]] = Set.empty
    var itemsInGroups: Set[A] = Set.empty
    val allItems: Set[A] = neighbors.keySet
    while (allItems.exists(p => !itemsInGroups.contains(p))) {
      val newGroup = reachableFrom(allItems.find(p => !itemsInGroups.contains(p)).get, neighbors)
      groups = groups + newGroup
      itemsInGroups = itemsInGroups ++ newGroup
    }
    groups
  }

  def parse(lines: Iterable[String]): Map[Int, Set[Int]] = {
    lines.map(parse).zipWithIndex.map(_.swap).toMap
  }

  def parse(line: String): Set[Int] = {
    line.split(" <-> ")(1).split(", ").toSet.map { s: String => s.toInt }
  }
}
