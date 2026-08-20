object DataDefs:
  type Pos  = (x: Int, y: Int)
  type Grid = Map[Pos, Int]

  val Directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  extension (p: Pos)
    def delta(dx: Int, dy: Int): Pos =
      (x = p.x + dx, y = p.y + dy)

  extension (grid: Grid)
    def neighbours(p: Pos): Seq[Pos] = Directions
      .map(p.delta)
      .filter(grid.contains)

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Grid = Seq
    .tabulate(lines.head.size, lines.size): (x, y) =>
      (x, y) -> lines(y)(x).asDigit
    .flatten
    .toMap

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def dijkstra(todo: Set[Pos], risk: Grid)(start: Pos, end: Pos, grid: Grid): Int =
    val point = todo.minBy(risk)
    if point == end then risk(end)
    else
      val (nextTodo, nextRisk) = grid
        .neighbours(point)
        .filter: next =>
          !risk.contains(next) || risk(point) + grid(next) < risk(next)
        .foldLeft((todo - point, risk)):
          case ((todo, curRisk), next) =>
            (todo + next, curRisk.updated(next, curRisk(point) + grid(next)))
      dijkstra(nextTodo, nextRisk)(start, end, grid)
  end dijkstra

  def path(grid: Grid): Int =
    val (start, end) = ((x = 0, y = 0), grid.keys.maxBy(p => p.x * p.y))
    dijkstra(Set(start), Map(start -> 0))(start, end, grid)

  def expand(grid: Grid): Grid =
    val end             = grid.keys.maxBy(p => p.x * p.y)
    val (width, height) = (end.x + 1, end.y + 1)
    Seq
      .tabulate(5, 5): (x, y) =>
        grid.toSeq.map: (pos, value) =>
          val newX = x * width + pos.x
          val newY = y * height + pos.y
          (x = newX, y = newY) -> (1 + (value - 1 + x + y) % 9)
      .flatten
      .flatten
      .toMap
  end expand

  def solve1(lines: Seq[String]) = path(Parsing.parse(lines))
  def solve2(lines: Seq[String]) = path(expand(Parsing.parse(lines)))

object Test:
  val file  = os.pwd / "2021" / "15" / "15.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 40
// Test.res2 // part 2: 315

object Main:
  val file  = os.pwd / "2021" / "15" / "15.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 717
// Main.res2 // part 2: 2993
