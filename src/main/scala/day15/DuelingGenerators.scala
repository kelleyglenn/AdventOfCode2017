package day15

object DuelingGenerators {
  def countOfPairsWithPredicates(pairs: Int, aStartingValue: Int, bStartingValue: Int): Int = {
    countOfPairsMatchingLowest2Bytes(pairs, aStartingValue, bStartingValue, _ % 4 == 0, _ % 8 == 0)
  }

  def countOfPairsMatchingLowest2Bytes(pairs: Int, aStartingValue: Int, bStartingValue: Int,
                                       aPredicate: Int => Boolean = { _ => true }, bPredicate: Int => Boolean = { _ => true }): Int = {
    val generatorA = Generator(aStartingValue, 16807, aPredicate)
    val generatorB = Generator(bStartingValue, 48271, bPredicate)
    var matching = 0
    (1 to pairs).foreach { _ =>
      val aValue = generatorA.nextValue
      val bValue = generatorB.nextValue
      if ((aValue & 0xFFFF) == (bValue & 0xFFFF)) matching += 1
    }
    matching
  }

  case class Generator(startingValue: Int, factor: Int, predicate: Int => Boolean) {
    var previousValue: Int = startingValue

    def nextValue: Int = {
      previousValue = ((previousValue.toLong * factor) % Int.MaxValue).toInt
      if (!predicate(previousValue)) nextValue else previousValue
    }
  }

}
