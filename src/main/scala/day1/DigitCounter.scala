package day1

object DigitCounter {
  def sumRepeatsNext(s: String): Int = {
    sumRepeats(s, 1)
  }

  def sumRepeatsOpposite(s: String): Int = {
    sumRepeats(s, s.length / 2)
  }

  def sumRepeats(s: String, skip: Int): Int = {
    var sum = 0
    val chars: Array[Char] = s.toCharArray
    chars.indices.foreach((i: Int) =>
      if (chars(i) == chars((i + skip) % chars.length))
        sum = sum + chars(i).asDigit)
    sum
  }
}
