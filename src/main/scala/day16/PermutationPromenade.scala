package day16

object PermutationPromenade {
  def dance(startingPrograms: IterableOnce[Char], moves: IterableOnce[String]): IterableOnce[Char] = {
    var programs = startingPrograms.iterator.toArray

    def spin(endCount: Int): Unit = {
      programs = programs.takeRight(endCount) ++ programs.take(programs.length - endCount)
    }

    def exchange(i1: Int, i2: Int): Unit = {
      val tempVal1 = programs(i1)
      programs(i1) = programs(i2)
      programs(i2) = tempVal1
    }

    def partner(p1: Char, p2: Char): Unit = {
      exchange(programs.indexOf(p1), programs.indexOf(p2))
    }

    moves.iterator.foreach { m =>
      m.head match {
        case 's' => spin(m.tail.toInt)
        case 'x' => exchange(m.tail.split('/')(0).toInt, m.tail.split('/')(1).toInt)
        case 'p' => partner(m.tail.split('/')(0).head, m.tail.split('/')(1).head)
      }
    }
    programs
  }
}
