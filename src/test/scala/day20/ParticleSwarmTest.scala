package day20

import org.scalatest.flatspec.AnyFlatSpec
import util.SetupPuzzleData

class ParticleSwarmTest extends AnyFlatSpec {
  behavior of "slowestMovingIndex"
  it should "handle the first example" in new SetupPuzzleData("example") {
    assert(ParticleSwarm.slowestMovingIndex(lines) == 0)
  }
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    assert(ParticleSwarm.slowestMovingIndex(lines) == 457)
  }
  behavior of "afterAllCollisions"
  it should "handle the second example" in new SetupPuzzleData("example2") {
    assert(ParticleSwarm.afterAllCollisions(lines).size == 1)
  }
  it should "solve the second puzzle" in new SetupPuzzleData("input") {
    assert(ParticleSwarm.afterAllCollisions(lines).size == 448)
  }
}
