package day21

object FractalArt {
  type Grid = Seq[String]

  def onCountAfterIters(start: Grid, rules: IterableOnce[String], iters: Int): Int = {
    gridAfterIters(start, rules, iters).map(s => s.count(_ == '#')).sum
  }

  def gridAfterIters(start: Grid, rules: IterableOnce[String], iters: Int): Grid = {
    val gridRules: Map[Grid, Grid] = parseRules(rules)
    var curGrid = start
    (1 to iters).foreach { _ =>
      val subGridSize: Short = if (curGrid.size % 2 == 0) 2 else 3
      val subGrids: Iterable[Iterable[Grid]] = splitGrid(curGrid, subGridSize)
      curGrid = joinGridOfGrids(subGrids.map { grids => grids.map { grid: Grid => gridRules(grid) } })
    }
    curGrid
  }

  private def parseRules(rules: IterableOnce[String]): Map[Grid, Grid] = {
    rules.iterator.foldLeft(Map[Grid, Grid]()) { case (map, rule) => map ++ parseRule(rule) }
  }

  private def parseRule(rule: String): Map[Grid, Grid] = {
    val grids = rule.split(" => ").map(_.split('/'))
    val from: Grid = grids(0)
    val to: Grid = grids(1)
    flipAndRotate(from).map(_ -> to).toMap
  }

  def flipAndRotate(v: Grid): Set[Grid] = {
    flip(v) ++ flip(rotate90(v))
  }

  private def rotate90(v: Grid): Grid = {
    v.head.indices.foldLeft(Vector[String]()) { case (g: Grid, i) =>
      val newString = v.indices.reverse.foldLeft(new StringBuilder) { case (builder: StringBuilder, j) =>
        builder.addOne(v(j)(i))
      }.mkString
      g.appended(newString)
    }
  }

  private def flip(v: Grid): Set[Grid] = {
    Set(v) + flipLR(v) + v.reverse + flipLR(v).reverse
  }

  private def flipLR(v: Grid): Grid = {
    v.map(_.reverse)
  }

  private def splitGrid(v: Grid, subGridSize: Short): Iterable[Iterable[Grid]] = {
    v.grouped(subGridSize).map { wideGrid =>
      val newGridsCount = wideGrid.head.length / subGridSize
      for (i <- 0 until newGridsCount) yield wideGrid.map(_.substring(subGridSize * i, subGridSize * (i + 1)))
    }.to(Iterable)
  }

  private def joinGridOfGrids(grids: Iterable[Iterable[Grid]]): Grid = {
    grids.foldLeft(Vector[String]()) { case (outputGrid: Grid, rowOfGrids: Iterable[Grid]) =>
      outputGrid ++ joinRowOfGrids(rowOfGrids)
    }
  }

  private def joinRowOfGrids(grids: Iterable[Grid]): Grid = {
    if (grids.iterator.length < 2) grids.head
    else grids.tail.foldLeft(grids.head) { case (result, next) => joinGrids(result, next) }
  }

  private def joinGrids(a: Grid, b: Grid): Grid = {
    a.indices.foldLeft(Vector[String]()) { case (result, i) => result.appended(a(i) + b(i)) }
  }
}
