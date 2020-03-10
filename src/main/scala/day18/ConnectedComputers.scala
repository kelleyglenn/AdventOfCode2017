package day18

import scala.collection.mutable

object ConnectedComputers {

  def sentCount(instructions: Seq[String]): (Int, Int) = {
    val comp0input: mutable.ArrayDeque[Long] = mutable.Queue[Long]()
    val comp1input: mutable.ArrayDeque[Long] = mutable.Queue[Long]()
    val comp0 = Computer(0, comp0input, comp1input, instructions)
    val comp1 = Computer(1, comp1input, comp0input, instructions)
    while (comp0.canRun || comp1.canRun) {
      while (comp0.canRun) comp0.run()
      while (comp1.canRun) comp1.run()
    }
    (comp0.sentCount, comp1.sentCount)
  }

  private case class Computer(id: Short, input: mutable.ArrayDeque[Long], output: mutable.ArrayDeque[Long], instructions: Seq[String]) {
    private val registers: mutable.Map[Char, Long] = mutable.Map('p' -> id.toLong).withDefaultValue(0)
    var sentCount = 0
    private var pc = 0

    def canRun: Boolean = {
      instructions.indices.contains(pc) && !(input.isEmpty && instructions(pc).startsWith("rcv"))
    }

    def run(): Unit = {
      instructions(pc).split(' ') match {
        case Array("snd", x) =>
          output.addOne(read(x))
          sentCount += 1
        case Array("set", x, y) => registers(x.head) = read(y)
        case Array("add", x, y) => registers(x.head) = registers(x.head) + read(y)
        case Array("mul", x, y) => registers(x.head) = registers(x.head) * read(y)
        case Array("mod", x, y) => registers(x.head) = registers(x.head) % read(y)
        case Array("rcv", x) => registers(x.head) = input.removeHead()
        case Array("jgz", x, y) => if (read(x) > 0) pc = (pc + read(y) - 1).toInt
        case _ => throw new IllegalArgumentException
      }
      pc += 1
    }

    private def read(s: String): Long = {
      if (('a' to 'z').contains(s.head.toLower)) registers(s.head) else s.toLong
    }
  }

}
