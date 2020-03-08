package day15

import org.scalatest.flatspec.AnyFlatSpec

class DuelingGeneratorsTest extends AnyFlatSpec {
  behavior of "countOfPairsMatchingLowest2Bytes"
  it should "handle the first example" in {
    assert(DuelingGenerators.countOfPairsMatchingLowest2Bytes(5, 65, 8921) == 1)
    assert(DuelingGenerators.countOfPairsMatchingLowest2Bytes(40000000, 65, 8921) == 588)
  }
  it should "solve the first puzzle" in {
    assert(DuelingGenerators.countOfPairsMatchingLowest2Bytes(40000000, 277, 349) == 592)
  }
  behavior of "countOfPairsWithPredicates"
  it should "handle the second example" in {
    assert(DuelingGenerators.countOfPairsWithPredicates(5000000, 65, 8921) == 309)
  }
  it should "solve the second puzzle" in {
    assert(DuelingGenerators.countOfPairsWithPredicates(5000000, 277, 349) == 320)
  }
}
