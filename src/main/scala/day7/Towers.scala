package day7

import scala.util.matching.Regex

object Towers {
  private val nameAndWeightGroup = raw"(\w+) \((\d+)\)"
  private val childrenGroup = raw" -> (.+)"
  private val childlessReg: Regex = nameAndWeightGroup.r("name", "weight")
  private val withChildReg: Regex = (nameAndWeightGroup + childrenGroup).r("name", "weight", "children")

  def nameOfBottomProgram(lines: Iterable[String]): String = {
    val programs: Set[Node] = parse(lines)
    programs.map(_.name).diff(programs.flatMap(_.children)).head
  }

  private def parse(lines: Iterable[String]): Set[Node] = {
    lines.foldLeft(Set[Node]()) {
      case (programs, line) => programs + parse(line)
    }
  }

  private def parse(line: String): Node = {
    if (withChildReg.matches(line)) {
      val groups: Regex.MatchIterator = withChildReg.findAllIn(line)
      Node(groups.group("name"), groups.group("weight").toInt, groups.group("children").split(", ").toSet)
    }
    else {
      val groups: Regex.MatchIterator = childlessReg.findAllIn(line)
      Node(groups.group("name"), groups.group("weight").toInt, Set.empty)
    }
  }

  def correctedWeightOfOffBalanceProgram(lines: Iterable[String]): Int = {
    val programs: Map[String, Node] = parse(lines).map((n: Node) => (n.name, n)).toMap
    val allParentPrograms: Iterable[Node] = programs.values.filter(_.children.nonEmpty)
    val allOffBalanceParentPrograms: Iterable[Node] =
      allParentPrograms.filter(_.children.map((child: String) => weightOfTowerCountingNode(programs(child), programs)).size != 1)
    val topOffBalanceParentProgram: Node =
      allOffBalanceParentPrograms.minBy((parent: Node) => weightOfTowerCountingNode(parent, programs))
    val weightsToChildren: Map[Int, List[Node]] =
      topOffBalanceParentProgram.children.toList.map((child: String) => programs(child)).groupBy((node: Node) => weightOfTowerCountingNode(node, programs))
    val oddChild: (Int, List[Node]) = weightsToChildren.find(_._2.size == 1).get
    val increment: Int = weightsToChildren.find(_._2.size > 1).get._1 - oddChild._1
    oddChild._2.head.weight + increment
  }

  private def weightOfTowerCountingNode(node: Node, programs: Map[String, Node]): Int = {
    node.weight + node.children.toList.map((childName: String) => weightOfTowerCountingNode(programs(childName), programs)).sum
  }

  case class Node(name: String, weight: Int, children: Set[String]) {}

}
