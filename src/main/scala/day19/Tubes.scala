package day19

object Tubes {
  def lettersEncounteredAndSteps(diagram: Vector[String]): (String, Int) = {
    val builder = new StringBuilder()
    val SPACE = ' '
    var x, y, steps = 0
    while (diagram(y)(x) != '|') x += 1
    var xDir = 0
    var yDir = 1
    while (diagram.indices.contains(y) && diagram(y).indices.contains(x) && diagram(y)(x) != SPACE) {
      val curChar = diagram(y)(x)
      if (curChar.isLetter) builder.addOne(curChar)
      else if (curChar == '+') {
        if (yDir == 0) {
          xDir = 0
          yDir = if (y > 0 && diagram(y - 1).indices.contains(x) && diagram(y - 1)(x) != SPACE) -1 else 1
        }
        else {
          yDir = 0
          xDir = if (x > 0 && diagram(y)(x - 1) != SPACE) -1 else 1
        }
      }
      x += xDir
      y += yDir
      steps += 1
    }
    (builder.mkString, steps)
  }
}
