package aoc2021.day09

object DataDefs:
  type Height = Int
  type Pos    = (x: Int, y: Int)
  type Grid   = Map[Pos, Height]

  extension (p: Pos) def delta(dp: Pos): Pos = (p.x + dp.x, p.y + dp.y)
  object Pos:
    val dirs: Seq[Pos] = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

  extension (g: Grid)
    def neighbors(p: Pos) = Pos.dirs.map(p.delta).filter(g.contains)
    def lowest(p: Pos)    = g.neighbors(p).map(g).forall(_ > g(p))
    def lowestPoss        = g.keys.toSeq.filter(g.lowest).map(g -> _)
    def risk(p: Pos)      = g(p) + 1
    def basinSize(lowest: Pos): Int =
      def visit(visited: Set[Pos], p: Pos): Set[Pos] =
        if visited.contains(p) || g(p) == 9 then visited
        else g.neighbors(p).foldLeft(visited + p)(visit)
      visit(Set(), lowest).size

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Grid = Seq
    .tabulate(lines.head.size, lines.size): (x, y) =>
      (x = x, y = y) -> lines(y)(x).asDigit
    .flatten
    .toMap

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .lowestPoss
    .map(_.risk(_))
    .sum

  def solve2(lines: Seq[String]) = Parsing
    .parse(lines)
    .lowestPoss
    .map(_.basinSize(_))
    .sorted
    .takeRight(3)
    .product

object Test:
  val file  = os.pwd / "2021" / "09" / "09.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2021" / "09" / "09.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 15
  println(Test.res2) // part 2: 1134
  println(Main.res1) // part 1: 456
  println(Main.res2) // part 2: 1047744
