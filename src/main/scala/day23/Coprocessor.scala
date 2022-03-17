package day23

import scala.collection.mutable

object Coprocessor {
  def timesMulInvoked(instructions: Seq[String]): Int = {
    var mulInvokedCt = 0

    def debug(pc: Int, op: String, x: String, y: String, registers: Map[Char, Long]): Unit = {
      if (op == "mul") mulInvokedCt += 1
    }

    val proc = new Processor(mutable.Map.empty.withDefaultValue(0), debug)
    proc.run(instructions)
    mulInvokedCt
  }

  def valueOfHWithDebugFlag(instructions: Seq[String], setDebugFlag: Boolean = true): Long = {
    def debug(pc: Int, op: String, x: String, y: String, registers: Map[Char, Long]): Unit = {
      if (pc == 27) println(pc.toString + ": " + instructions(pc) + " == " + registers)
    }

    val proc = new Processor(mutable.Map('a' -> (if (setDebugFlag) 1.toLong else 0.toLong)).withDefaultValue(0), debug)
    val finalRegisters: Map[Char, Long] = proc.run(instructions)
    finalRegisters('h')
  }

  private def debugNothing(pc: Int, op: String, x: String, y: String, registers: Map[Char, Long]): Unit = {}

  trait Instruction {
    val line: Int
    val x: String
    val y: String
  }

  class Processor(registers: mutable.Map[Char, Long],
                  debugAfter: (Int, String, String, String, Map[Char, Long]) => Unit = debugNothing,
                  startPC: Int = 0) {
    def run(instructions: Seq[String]): Map[Char, Long] = {
      var pc: Int = startPC

      def read(s: String): Long = {
        if (('a' to 'z').contains(s.head.toLower)) registers(s.head) else s.toLong
      }

      while (instructions.indices.contains(pc)) {
        var pcInc = 1
        instructions(pc).split(' ') match {
          case Array(op, x, y) =>
            op match {
              case "set" => registers(x.head) = read(y)
              case "sub" => registers(x.head) = registers(x.head) - read(y)
              case "mul" => registers(x.head) = registers(x.head) * read(y)
              case "jnz" => if (read(x) != 0) pcInc = read(y).toInt
              case _ => throw new IllegalArgumentException
            }
            debugAfter(pc, op, x, y, registers.toMap)
        }
        pc += pcInc
      }
      registers.toMap
    }
  }

  object Op extends Enumeration {
    val SET, SUB, MUL, JNZ = Value
  }

}
