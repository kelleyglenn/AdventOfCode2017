package day20

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class ParticleSwarmTest extends AnyFlatSpec {
  behavior of "slowestMovingIndex"
  it should "handle the example" in new SetupPuzzleData("example") {
    assert(ParticleSwarm.slowestMovingIndex(lines) == 0)
  }
  it should "solve the puzzle" in new SetupPuzzleData("input") {
    assert(ParticleSwarm.slowestMovingIndex(lines) == 457)
  }
}
