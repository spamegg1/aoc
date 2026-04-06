package aoc2025.day8

import math.{pow, sqrt}

val file  = os.pwd / "2025" / "08" / "08.input.txt"
val lines = os.read.lines(file)

case class Coord(x: Int, y: Int, z: Int):
  def distance(other: Coord): Double = sqrt(distancesq(other))
  // unused but would be slightly faster, suitable for sorting by distance
  def distancesq(other: Coord): Double =
    pow(x - other.x, 2) + pow(y - other.y, 2) + pow(z - other.z, 2)

// all connectors
lazy val connectors: Set[Coord] = lines.map { case s"$x,$y,$z" =>
  Coord(x.toInt, y.toInt, z.toInt)
}.toSet

// all pairs of connectors sorted by distance
lazy val shortestConnections: Vector[(Coord, Coord)] =
  connectors.toVector
    .combinations(2)
    .map: coords =>
      (coords, coords(0).distance(coords(1)))
    .toVector
    .sortBy(_._2)
    .map(_._1)
    .map(xs => (xs(0), xs(1)))

def mergePair(pair: (Coord, Coord), networks: Set[Set[Coord]]): Set[Set[Coord]] =
  // find networks to merge together
  val merge = networks.filter(ntwk => ntwk.contains(pair._1) || ntwk.contains(pair._2))
  // remove those networks, merge them into one network (including up to 2 new ones), add the new merged network
  // keep in mind this works even when "networks" is empty
  (networks -- merge) + (merge.flatten + pair._1 + pair._2)

@main def s8_1(): Unit = println {
  shortestConnections
    .take(1000)
    .foldRight(Set[Set[Coord]]()):
      case (pair, accum) => mergePair(pair, accum)
    .toVector
    .sortBy(_.size)
    .takeRight(3)
    .map(_.size)
    .product
}

@main def s8_2(): Unit = println {
  case class State(shortest: Vector[(Coord, Coord)], networks: Set[Set[Coord]])
  LazyList
    .unfold(State(shortestConnections, Set.empty)) { case State(shortest, networks) =>
      // if networks is 1 merged set containing all connections
      if networks.size == 1 && networks.head == connectors then None
      else Some(shortest.head, State(shortest.tail, mergePair(shortest.head, networks)))
    }
    .last
    .toList
    .map(_.x.toLong)
    .product
}
