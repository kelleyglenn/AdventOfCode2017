package day20

object ParticleSwarm {
  def slowestMovingIndex(lines: IterableOnce[String]): Int = {
    val maxParticle = Particle(
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue))
    parse(lines).zipWithIndex.toSet.fold((maxParticle, 0)) { case ((minP, minI), (p, i)) =>
      if ((p.acceleration.distance < minP.acceleration.distance) ||
        (p.acceleration.distance == minP.acceleration.distance && p.velocity.distance < minP.velocity.distance) ||
        (p.acceleration.distance == minP.acceleration.distance &&
          p.velocity.distance == minP.velocity.distance &&
          p.pos.distance < minP.pos.distance)) (p, i)
      else (minP, minI)
    }._2
  }

  def parse(lines: IterableOnce[String]): Iterator[Particle] = {
    lines.iterator.map(parseParticle)
  }

  def parseParticle(line: String): Particle = {
    val particleGroups = raw"p=<(.*)>, v=<(.*)>, a=<(.*)>".r("p", "v", "a").findAllIn(line)
    Particle(parsePos(particleGroups.group("p")), parsePos(particleGroups.group("v")), parsePos(particleGroups.group("a")))
  }

  def parsePos(s: String): Pos = {
    val xyzGroups = raw"(-?\d+),(-?\d+),(-?\d+)".r("x", "y", "z").findAllIn(s)
    Pos(xyzGroups.group("x").toInt, xyzGroups.group("y").toInt, xyzGroups.group("z").toInt)
  }

  case class Particle(pos: Pos, velocity: Pos, acceleration: Pos)

  case class Pos(x: Int, y: Int, z: Int) {
    def distance: Int = {
      Math.abs(x) + Math.abs(y) + Math.abs(z)
    }
  }

}
