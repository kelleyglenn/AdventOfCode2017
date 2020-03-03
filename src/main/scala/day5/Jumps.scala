package day5

object Jumps {
  def countJumpsUntilExit(offsets: Array[Int], inc: Int => Int): Int = {
    var jumpCount = 0
    var i = 0
    while (offsets.indices.contains(i)) {
      val newI = i + offsets(i)
      offsets(i) += inc(offsets(i))
      i = newI
      jumpCount += 1
    }
    jumpCount
  }
}
