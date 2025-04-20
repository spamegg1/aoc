package aoc2019.day18

import collection.mutable.{Map => MMap, PriorityQueue => PQueue}

object DataDefs:
  type Pos     = (x: Int, y: Int)
  type Tunnels = Map[Pos, Char]
  type Keys    = Set[Pos]
  type Robots  = Set[Pos]

  extension (p: Pos)
    def delta(dx: Int, dy: Int): Pos = (p.x + dx, p.y + dy)
    def neighbors = Seq(p.delta(0, -1), p.delta(0, 1), p.delta(-1, 0), p.delta(1, 0))

  case class Move(from: Pos, to: Pos, cost: Int, neededKeys: Set[Char])

  extension (c: Char)
    def isRobot = c == '@'
    def isWall  = c == '#'
    def isKey   = 'a' <= c && c <= 'z'
    def isDoor  = 'A' <= c && c <= 'Z'

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): (Tunnels, Keys, Robots) =
    val tunnels =
      for
        y <- lines.indices
        x <- lines.head.indices
      yield (x = x, y = y) -> lines(y)(x)
    val remainingKeys = tunnels.filter((_, letter) => letter.isKey).map(_._1)
    val robots        = tunnels.filter((_, letter) => letter.isRobot).map(_._1)
    (tunnels.toMap, remainingKeys.toSet, robots.toSet)

object Solving:
  import DataDefs.*

  def dijkstra(tunnels: Tunnels, start: Pos): Seq[Move] =
    val cost       = MMap(start -> 0)
    val keysNeeded = MMap(start -> Set.empty[Char])
    val todo       = PQueue(start)(Ordering.by(cost))

    while todo.nonEmpty do
      val pos = todo.dequeue()
      pos.neighbors.view
        .filter(neighbor => !tunnels(neighbor).isWall)
        .filter(neighbor => !cost.contains(neighbor) || cost(pos) + 1 < cost(neighbor))
        .foreach: neighbor =>
          cost(neighbor) = cost(pos) + 1
          keysNeeded(neighbor) =
            if tunnels(neighbor).isDoor
            then keysNeeded(pos) + tunnels(neighbor).toLower
            else keysNeeded(pos)
          todo.enqueue(neighbor)

    cost.keys
      .map(p => Move(start, p, cost(p), keysNeeded(p)))
      .toSeq

  def explore(tunnels: Tunnels, remainingKeys: Keys, robots: Robots): Int =
    val cache = MMap[(Set[Pos], Set[Char]), Int]()

    val routes = (remainingKeys ++ robots)
      .flatMap(pos => dijkstra(tunnels, pos))
      .map(move => (move.from, move.to) -> move)
      .toMap

    def helper(
        remaining: Set[Pos],
        robots: Set[Pos],
        collected: Set[Char],
        total: Int,
        result: Int
    ): Int =
      val cachedRes = cache.getOrElse((robots, collected), Int.MaxValue)
      if total >= result || total >= cachedRes then result
      else if remaining.isEmpty then total
      else
        cache((robots, collected)) = total
        val candidates =
          for
            from <- robots
            to   <- remaining
          yield routes.get((from, to))

        candidates.toSeq.flatten
          .filter(_.neededKeys.subsetOf(collected))
          .sortBy(_.cost)
          .foldLeft(result):
            case (result, Move(from, to, cost, _)) =>
              helper(
                remaining - to,
                robots - from + to,
                collected + tunnels(to),
                total + cost,
                result
              )
    end helper
    helper(remainingKeys, robots, Set(), 0, Int.MaxValue)

  def solve(lines: Seq[String]) =
    val (tunnels, remainingKeys, robots) = Parsing.parse(lines)
    explore(tunnels, remainingKeys, robots)

object Test:
  lazy val path   = os.pwd / "2019" / "18"
  lazy val file1  = path / "18.test.input.1.txt"
  lazy val file2  = path / "18.test.input.2.txt"
  lazy val file3  = path / "18.test.input.3.txt"
  lazy val file4  = path / "18.test.input.4.txt"
  lazy val file5  = path / "18.test.input.5.txt"
  lazy val file6  = path / "18.test.input.6.txt"
  lazy val file7  = path / "18.test.input.7.txt"
  lazy val file8  = path / "18.test.input.8.txt"
  lazy val file9  = path / "18.test.input.9.txt"
  lazy val files1 = Seq(file1, file2, file3, file4, file5)
  lazy val files2 = Seq(file6, file7, file8, file9)
  lazy val lines1 = files1 map os.read.lines
  lazy val lines2 = files2 map os.read.lines
  lazy val res1   = lines1 map Solving.solve
  lazy val res2   = lines2 map Solving.solve

object Main:
  lazy val file1 = os.pwd / "2019" / "18" / "18.input.txt"
  lazy val file2 = os.pwd / "2019" / "18" / "18.input.patched.txt"
  lazy val line1 = os.read.lines(file1)
  lazy val line2 = os.read.lines(file2)
  lazy val res1  = Solving.solve(line1)
  lazy val res2  = Solving.solve(line2)

@main
def run: Unit =
  println(Test.res1) // part 1: 8,86,132,136,81
  println(Test.res2) // part 2: 8,24,32,72
  println(Main.res1) // part 1: 3764
  println(Main.res2) // part 2: 1738
