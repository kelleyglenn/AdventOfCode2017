package day12

object Plumber {
  def reachableFrom0Count(neighbors: Array[Set[Int]]): Int = {
    reachableFrom(0, neighbors).size
  }

  private def reachableFrom(from: Int, neighbors: Array[Set[Int]]): Set[Int] = {
    var visited: Set[Int] = Set(from)
    var visitedWithUnvisitedNeighbors = visited.find(i => neighbors(i).exists(n => !visited.contains(n)))
    while (visitedWithUnvisitedNeighbors.nonEmpty) {
      visited = visited ++ neighbors(visitedWithUnvisitedNeighbors.get)
      visitedWithUnvisitedNeighbors = visited.find(i => neighbors(i).exists(n => !visited.contains(n)))
    }
    visited
  }

  def groups(neighbors: Array[Set[Int]]): Set[Set[Int]] = {
    var groups: Set[Set[Int]] = Set.empty
    var programsInGroups: Set[Int] = Set.empty
    val allPrograms: Set[Int] = neighbors.indices.toSet
    while (allPrograms.exists(p => !programsInGroups.contains(p))) {
      val newGroup = reachableFrom(allPrograms.find(p => !programsInGroups.contains(p)).get, neighbors)
      groups = groups + newGroup
      programsInGroups = programsInGroups ++ newGroup
    }
    groups
  }

  def parse(lines: Iterable[String]): Array[Set[Int]] = {
    lines.map(parse).toArray
  }

  def parse(line: String): Set[Int] = {
    line.split(" <-> ")(1).split(", ").toSet.map { s: String => s.toInt }
  }
}
