package aoc2016.day24

object DataDefs:
  type Steps = Int
  type Loc   = Int             // the numbers in puzzle input, locations of interest
  type Route = Seq[Loc]        // a permutation of those locations
  type Dists = Map[Loc, Steps] // distances from one location to all others
  type Pos   = (x: Int, y: Int)
  type Grid  = Set[Pos]        // Seq = bad performance, Set = great performance!

  extension (p: Pos)
    def neighbors = Seq(
      (x = p.x - 1, y = p.y),
      (x = p.x + 1, y = p.y),
      (x = p.x, y = p.y - 1),
      (x = p.x, y = p.y + 1)
    )

object Parsing:
  import DataDefs.*

  def parseLocs(lines: Seq[String]): Map[Loc, Pos] =
    (for
      row <- 0 until lines.size
      col <- 0 until lines.head.size
      if lines(row)(col).isDigit
    yield lines(row)(col).asDigit -> (col, row)).toMap

  def parseGrid(lines: Seq[String]): Grid =
    (for
      row <- 0 until lines.size
      col <- 0 until lines.head.size
      if lines(row)(col) != '#'
    yield (col, row)).toSet

  def parse(lines: Seq[String]) = (parseLocs(lines), parseGrid(lines))

object Solving:
  import DataDefs.*

  def bfs(grid: Grid, start: Pos, end: Pos): Steps =
    import collection.mutable.{Map, Queue}, util.boundary, boundary.break

    val steps = Map(start -> 0)
    val queue = Queue(start)
    var res   = -1

    boundary:
      while queue.nonEmpty do
        val current = queue.dequeue()
        if current == end then
          res = steps(current)
          break()
        val newSteps = steps(current) + 1

        for neighbor <- current.neighbors.filter(grid.contains) do
          if !steps.contains(neighbor) || steps(neighbor) > newSteps then
            steps(neighbor) = newSteps
            queue.enqueue(neighbor)
    res

  def makeGraph(locs: Map[Loc, Pos], grid: Grid): Map[Loc, Dists] =
    locs.transform: (_, start) =>
      locs.transform: (_, end) =>
        bfs(grid, start, end)

  def travelingSalesman(graph: Map[Loc, Dists], routes: Seq[Route]): Steps =
    routes
      .map: route =>
        route
          .sliding(2)
          .map(next => graph(next.head)(next.last))
          .sum
      .min

  def solve1(lines: Seq[String]) =
    val (locs, grid) = Parsing.parse(lines)
    val graph        = makeGraph(locs, grid)
    val routes       = (1 to graph.keys.max).permutations.map(_.prepended(0)).toSeq
    travelingSalesman(graph, routes)

  def solve2(lines: Seq[String]) =
    val (locs, grid) = Parsing.parse(lines)
    val graph        = makeGraph(locs, grid)
    val routes = (1 to graph.keys.max).permutations
      .map(_.prepended(0).appended(0))
      .toSeq
    travelingSalesman(graph, routes)

object Test:
  lazy val file  = os.pwd / "2016" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)

object Main:
  lazy val file  = os.pwd / "2016" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 14
  println(Main.res1) // part 1: 470
  println(Main.res2) // part 2: 720
