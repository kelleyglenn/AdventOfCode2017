package day10

object KnotHash {
  def hash(count: Int, swapsString: String): String = {
    val swaps: List[Int] = swapsString.map((c: Char) => c.toInt).toList ++ List(17, 31, 73, 47, 23)
    val numbers = new LoopedNumbers(count)
    (1 to 64).foreach { _: Int =>
      swapAndSkip(numbers, swaps)
    }
    val sparseHash: List[Int] = numbers.numbers.toList
    val denseHash: List[Int] = sparseHash.grouped(Math.sqrt(count).toInt).toList.map(xor)
    hexString(denseHash)
  }

  def hexString(denseHash: List[Int]): String = {
    denseHash.map((i: Int) => ("0" + i.toHexString).takeRight(2)).mkString
  }

  def xor(l: List[Int]): Int = {
    l.foldLeft(0) { case (result, number) => result ^ number }
  }

  def firstTwoNumbersAfterSwaps(count: Int, swaps: Iterable[Int]): (Int, Int) = {
    val list = new LoopedNumbers(count)
    swapAndSkip(list, swaps)
    (list.numbers(0), list.numbers(1))
  }

  private def swapAndSkip(linkedList: LoopedNumbers, swaps: Iterable[Int]): Unit = {
    swaps.foreach((swap: Int) => {
      linkedList.swapAndSkip(swap)
      linkedList.skip()
    })
  }

  class LoopedNumbers(count: Int) {
    val numbers: Array[Int] = (0 until count).toArray
    var curPos = 0
    var skipSize = 0

    def swapAndSkip(count: Int): Unit = {
      var start: Int = curPos
      var end: Int = fixIndex(start + count - 1)
      (1 to (count / 2)).foreach { _: Int =>
        val startingNumber: Int = numbers(start)
        numbers(start) = numbers(end)
        numbers(end) = startingNumber
        start = fixIndex(start + 1)
        end = fixIndex(end - 1)
      }
      skip(count)
    }

    def skip(): Unit = {
      skip(skipSize)
      skipSize += 1
    }

    private def fixIndex(i: Int): Int = {
      if (i < 0) i + numbers.length
      else if (i >= numbers.length) i % numbers.length
      else i
    }

    private def skip(count: Int): Unit = {
      curPos = fixIndex(curPos + count)
    }
  }

}
