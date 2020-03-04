package day8

import day8.Comparator.ComparatorValue

class RegisterInstructions(instructions: Iterable[Instruction]) {
  private var registers: Map[String, Int] = Map.empty.withDefaultValue(0)

  def run(): (Map[String, Int], Int) = {
    var maxValue: Int = Int.MinValue
    instructions.foreach {
      case Instruction(op, cond) => if (cond.passes(registers)) {
        val newVal: Int = registers(op.register) + (if (op.inc) op.amount else -op.amount)
        registers = (registers + (op.register -> newVal)).withDefaultValue(0)
        maxValue = Math.max(maxValue, newVal)
      }
    }
    (registers, maxValue)
  }
}

case class Instruction(operation: Operation, condition: Condition)

object Instruction {
  val comparators: Map[String, ComparatorValue] = Map(
    ">" -> Comparator.GT,
    "<" -> Comparator.LT,
    ">=" -> Comparator.GTE,
    "<=" -> Comparator.LTE,
    "==" -> Comparator.EQ,
    "!=" -> Comparator.NEQ
  )

  def parse(lines: Iterable[String]): Iterable[Instruction] = {
    lines.map(parse)
  }

  def parse(line: String): Instruction = {
    val parts: Array[String] = line.split(' ')
    Instruction(Operation(parts(0), parts(1) == "inc", parts(2).toInt), Condition(parts(4), comparators(parts(5)), parts(6).toInt))
  }
}

case class Operation(register: String, inc: Boolean, amount: Int)

case class Condition(register: String, cmp: ComparatorValue, source: Int) {
  val compareToValues: Map[ComparatorValue, Set[Int]] = Map(
    Comparator.GT -> Set(1),
    Comparator.LT -> Set(-1),
    Comparator.GTE -> Set(0, 1),
    Comparator.LTE -> Set(-1, 0),
    Comparator.EQ -> Set(0),
    Comparator.NEQ -> Set(-1, 1)
  )

  def passes(registers: Map[String, Int]): Boolean = {
    compareToValues(cmp).contains(registers(register).compareTo(source))
  }
}

object Comparator extends Enumeration {
  type ComparatorValue = Value
  val GT, LT, GTE, LTE, EQ, NEQ = Value
}
