package day21

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class FractalArtTest extends AnyFlatSpec {
  val start = Vector(".#.", "..#", "###")
  behavior of "flipAndRotate"
  it should "return all permutations" in {
    assert(FractalArt.flipAndRotate(Vector("ABC", "DEF", "GHI")) ==
      Set(
        Vector("ABC", "DEF", "GHI"), // normal
        Vector("CBA", "FED", "IHG"), // flipped LR
        Vector("GHI", "DEF", "ABC"), // flipped UD
        Vector("IHG", "FED", "CBA"), // flipped LR && UD, same as rotate 180
        Vector("GDA", "HEB", "IFC"), // rotated 90
        Vector("ADG", "BEH", "CFI"), // rotated 90 and flipped LR
        Vector("IFC", "HEB", "GDA"), // rotated 90 and flipped UD
        Vector("CFI", "BEH", "ADG") // rotated -90, same as rotated 90 and flipped LR & UD
      ))
  }
  behavior of "gridAfterIters"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(FractalArt.gridAfterIters(start, lines, 2) == Vector(
      "##.##.",
      "#..#..",
      "......",
      "##.##.",
      "#..#..",
      "......"
    ))
  }
  behavior of "onCountAfterIters"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(FractalArt.onCountAfterIters(start, lines, 2) == 12)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(FractalArt.onCountAfterIters(start, lines, 5) == 173)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(FractalArt.onCountAfterIters(start, lines, 18) == 2456178)
  }
}
