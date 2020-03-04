package day9

object StreamProcessing {
  def scoreWithGarbageCount(stream: String): (Int, Int) = {
    var score: Int = 0
    var garbageCount = 0
    var curLevel = 0
    var skipNextChar = false
    var readingGarbage = false
    stream.foreach((c: Char) => {
      if (skipNextChar) skipNextChar = false
      else {
        c match {
          case '!' => skipNextChar = true
          case '{' if !readingGarbage =>
            curLevel += 1
            score += curLevel
          case '}' if !readingGarbage => curLevel -= 1
          case '<' if !readingGarbage => readingGarbage = true
          case '>' => readingGarbage = false
          case _ if readingGarbage => garbageCount += 1
          case _ => // do nothing
        }
      }
    })
    (score, garbageCount)
  }
}
