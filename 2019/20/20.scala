package aoc2019.day20

import collection.mutable.{Map => MMap, Queue => MQueue}

object DataDefs:
  type Pos = (x: Int, y: Int)
  extension (p: Pos) def delta(dx: Int, dy: Int): Pos = (x = p.x + dx, y = p.y + dy)

  val PATTERNS = Seq(
    Seq((-2, 0), (-1, 0)),
    Seq((1, 0), (2, 0)),
    Seq((0, -2), (0, -1)),
    Seq((0, 1), (0, 2))
  )
  val NEIGHBORS = Seq((-1, 0), (1, 0), (0, -1), (0, 1))

  enum Portal:
    case Start
    case End
    case Outer(label: String)
    case Inner(label: String)

  type Cost     = Int
  type Costs    = Map[Pos, Cost]
  type Path     = Seq[(Portal, Cost)]
  type PathSet  = Set[(Portal, Cost)]
  type State    = (path: Path, cost: Cost)
  type Routes   = Map[Portal, Path]
  type RouteSet = Map[Portal, PathSet]
  type Maze     = Map[Pos, Char]
  type Portals  = Map[Pos, Portal]

object BFS:
  import DataDefs.*, Portal.*

  def findPortals(width: Int, height: Int, maze: Maze): Portals =
    val portals =
      for
        x       <- 2 to width
        y       <- 2 to height
        pattern <- PATTERNS
      yield
        val pos                = (x, y)
        val Seq(first, second) = pattern.map(pos.delta).map(maze)
        if maze(pos) == '.' && first.isLetter && second.isLetter then
          val label = s"$first$second"
          val key =
            if label == "AA" then Start
            else if label == "ZZ" then End
            else if x == 2 || y == 2 || x == width || y == height then Outer(label)
            else Inner(label)
          Some(pos -> key)
        else None
    portals.flatten.toMap

  def bfs(start: Pos, maze: Maze): Costs =
    val cost = MMap(start -> 0)
    val todo = MQueue(start)

    while todo.nonEmpty do
      val point = todo.dequeue()
      NEIGHBORS
        .map(point.delta)
        .filter(next => maze(next) == '.')
        .filter(next => cost(point) + 1 < cost.getOrElse(next, Int.MaxValue))
        .foreach: next =>
          cost(next) = cost(point) + 1
          todo.enqueue(next)
    cost.toMap

object Parsing:
  import DataDefs.*, Portal.*

  def parse(lines: Seq[String]): RouteSet =
    val (width, height) = (lines.head.size, lines.size)
    val maze = Seq
      .tabulate(width, height)((x, y) => (x = x, y = y) -> lines(y)(x))
      .flatten
      .toMap
    val portals = BFS.findPortals(width - 3, height - 3, maze)

    val routes =
      for (start, portal) <- portals
      yield
        val cost       = BFS.bfs(start, maze)
        val candidates = cost.keySet.intersect(portals.keySet) - start
        portal -> candidates.map(point => portals(point) -> cost(point))

    routes.map: (portal, routes) =>
      val linkedRoutes = routes.map:
        case (Outer(label), cost) => (Inner(label), cost + 1)
        case (Inner(label), cost) => (Outer(label), cost + 1)
        case other                => other
      portal -> linkedRoutes

object Solving:
  import DataDefs.*, Portal.*

  def explore(routes: RouteSet, recursive: Boolean): Int =
    val todo   = MQueue((path = Seq(Start -> 0), cost = 0))
    var result = Int.MaxValue

    while todo.nonEmpty do
      val (path, total)    = todo.dequeue()
      val (current, depth) = path.last

      if total >= result then ()
      else if current == End then result = total
      else
        routes(current)
          .filter: (portal, cost) =>
            if !recursive then true
            else
              portal match
                case Inner(_) if depth == 0   => false
                case Start | End if depth > 0 => false
                case _                        => true
          .foreach: (portal, cost) =>
            val next = portal match
              case outer: Outer => (outer, depth + 1)
              case inner: Inner => (inner, depth - 1)
              case other        => (other, depth)
            if !path.contains(next) then
              todo.enqueue((path = path.appended(next), cost = total + cost))
    result

  def solve1(lines: Seq[String]): Int = explore(Parsing.parse(lines), false)
  def solve2(lines: Seq[String]): Int = explore(Parsing.parse(lines), true)

object Test:
  lazy val file1  = os.pwd / "2019" / "20" / "20.test.input.1.txt"
  lazy val file2  = os.pwd / "2019" / "20" / "20.test.input.2.txt"
  lazy val file3  = os.pwd / "2019" / "20" / "20.test.input.3.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val lines3 = os.read.lines(file3)
  lazy val res11  = Solving.solve1(lines1)
  lazy val res12  = Solving.solve1(lines2)
  lazy val res2   = Solving.solve2(lines3)

object Main:
  lazy val file  = os.pwd / "2019" / "20" / "20.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res11) // part 1: 23
  println(Test.res12) // part 1: 58
  println(Test.res2)  // part 2: 396
  println(Main.res1)  // part 1: 636
  println(Main.res2)  // part 2: 7248
