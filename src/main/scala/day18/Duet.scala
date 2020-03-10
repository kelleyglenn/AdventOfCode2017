package day18

import scala.collection.mutable

object Duet {
  def firstRecoveredFrequency(instructions: Seq[String]): Option[Long] = {
    var pc = 0
    val registers: mutable.Map[Char, Long] = mutable.Map.empty.withDefaultValue(0)
    var savedFreq: Option[Long] = None

    def read(s: String): Long = {
      if (('a' to 'z').contains(s.head.toLower)) registers(s.head) else s.toLong
    }

    while (instructions.indices.contains(pc)) {
      instructions(pc).split(' ') match {
        case Array("snd", x) => savedFreq = Some(read(x))
        case Array("set", x, y) => registers(x.head) = read(y)
        case Array("add", x, y) => registers(x.head) = registers(x.head) + read(y)
        case Array("mul", x, y) => registers(x.head) = registers(x.head) * read(y)
        case Array("mod", x, y) => registers(x.head) = registers(x.head) % read(y)
        case Array("rcv", x) => if (read(x) != 0) return savedFreq
        case Array("jgz", x, y) => if (read(x) > 0) pc = (pc + read(y) - 1).toInt
        case _ => throw new IllegalArgumentException
      }
      pc += 1
    }
    None
  }
}