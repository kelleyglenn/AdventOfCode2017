package day20

import scala.util.matching.Regex.MatchIterator

object ParticleSwarm {
  def slowestMovingIndex(lines: IterableOnce[String]): Int = {
    val maxParticle: Particle = Particle(Short.MaxValue,
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Pos(Int.MaxValue, Int.MaxValue, Int.MaxValue))
    val slowestParticle: Particle = parse(lines).fold(maxParticle) { case (minP, p) =>
      if ((p.acceleration.distance0 < minP.acceleration.distance0) ||
        (p.acceleration.distance0 == minP.acceleration.distance0 && p.velocity.distance0 < minP.velocity.distance0) ||
        (p.acceleration.distance0 == minP.acceleration.distance0 &&
          p.velocity.distance0 == minP.velocity.distance0 &&
          p.pos.distance0 < minP.pos.distance0)) p
      else minP
    }
    slowestParticle.id
  }

  private def parse(lines: IterableOnce[String]): Iterable[Particle] = {
    lines.iterator.zipWithIndex.map((si: (String, Int)) => parseParticle(si._1, si._2.toShort)).to(Iterable)
  }

  private def parseParticle(line: String, id: Short): Particle = {
    val particleGroups: MatchIterator = raw"p=<(.*)>, v=<(.*)>, a=<(.*)>".r("p", "v", "a").findAllIn(line)
    Particle(id, parsePos(particleGroups.group("p")), parsePos(particleGroups.group("v")), parsePos(particleGroups.group("a")))
  }

  private def parsePos(s: String): Pos = {
    val xyzGroups: MatchIterator = raw"(-?\d+),(-?\d+),(-?\d+)".r("x", "y", "z").findAllIn(s)
    Pos(xyzGroups.group("x").toInt, xyzGroups.group("y").toInt, xyzGroups.group("z").toInt)
  }

  def afterAllCollisions(lines: IterableOnce[String]): Set[Particle] = {
    var prevSurvivingParticles: Iterable[Particle] = parse(lines)
    var prevDistances: Map[Short, Map[Short, Int]] = distances(prevSurvivingParticles)
    var newSurvivingParticles: Iterable[Particle] = tick(prevSurvivingParticles)
    var newDistances: Map[Short, Map[Short, Int]] = distances(newSurvivingParticles)
    while (newSurvivingParticles.size < prevSurvivingParticles.size || anyCloser(newDistances, prevDistances)) {
      prevSurvivingParticles = newSurvivingParticles
      prevDistances = newDistances
      newSurvivingParticles = tick(prevSurvivingParticles)
      newDistances = distances(newSurvivingParticles)
    }
    prevSurvivingParticles.toSet
  }

  private def tick(particles: Iterable[Particle]): Iterable[Particle] = {
    val newParticles: Iterable[Particle] = particles.map { case Particle(id, p, v, a) =>
      val newV: Pos = v + a
      Particle(id, Pos(p.x + newV.x, p.y + newV.y, p.z + newV.z), newV, a)
    }
    removeCollisions(newParticles)
  }

  private def anyCloser(newDistances: Map[Short, Map[Short, Int]], prevDistances: Map[Short, Map[Short, Int]]): Boolean = {
    prevDistances.keySet.exists((id: Short) => anyCloserToParticle(newDistances(id), prevDistances(id)))
  }

  private def anyCloserToParticle(newDistances: Map[Short, Int], prevDistances: Map[Short, Int]): Boolean = {
    prevDistances.keySet.exists((id: Short) => newDistances(id) < prevDistances(id))
  }

  private def removeCollisions(particles: Iterable[Particle]): Iterable[Particle] = {
    val particlePosGroups: Map[Pos, Iterable[Particle]] = particles.groupBy(_.pos)
    particles.filter((p: Particle) => particlePosGroups(p.pos).size == 1)
  }

  private def distances(particles: Iterable[Particle]): Map[Short, Map[Short, Int]] = {
    if (particles.isEmpty) Map.empty
    else distances(particles.tail) + (particles.head.id -> particles.head.distanceTo(particles.tail))
  }

  case class Particle(id: Short, pos: Pos, velocity: Pos, acceleration: Pos) {
    def distanceTo(others: IterableOnce[Particle]): Map[Short, Int] = {
      others.iterator.map((o: Particle) => (o.id, distanceTo(o))).toMap
    }

    def distanceTo(other: Particle): Int = {
      pos.distanceTo(other.pos)
    }
  }

  case class Pos(x: Int, y: Int, z: Int) {
    def distance0: Int = {
      Math.abs(x) + Math.abs(y) + Math.abs(z)
    }

    def distanceTo(other: Pos): Int = {
      Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
    }

    def +(other: Pos): Pos = {
      Pos(x + other.x, y + other.y, z + other.z)
    }
  }

}
