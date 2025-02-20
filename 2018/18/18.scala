package aoc2018.day18

object DataDefs:
  type Pos  = (x: Int, y: Int)
  type Grid = Map[Pos, Char]

  extension (p: Pos)
    def neighbors = Seq(
      (p.x - 1, p.y - 1),
      (p.x, p.y - 1),
      (p.x + 1, p.y - 1),
      (p.x - 1, p.y),
      // (p.x, p.y),
      (p.x + 1, p.y),
      (p.x - 1, p.y + 1),
      (p.x, p.y + 1),
      (p.x + 1, p.y + 1)
    )

  extension (g: Grid)
    def score: Int             = g.values.count(_ == '|') * g.values.count(_ == '#')
    def count(p: Pos, c: Char) = p.neighbors.flatMap(g.get).count(_ == c)
    def next(p: Pos, c: Char) = c match
      case '.' => if g.count(p, '|') >= 3 then '|' else '.'
      case '|' => if g.count(p, '#') >= 3 then '#' else '|'
      case '#' => if g.count(p, '#') >= 1 && g.count(p, '|') >= 1 then '#' else '.'
    def step: Grid = g.map((pos, value) => pos -> next(pos, value))

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Grid =
    (for
      y <- lines.indices
      x <- lines.head.indices
    yield (x, y) -> lines(y)(x)).toMap

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Iterator
    .iterate(Parsing.parse(lines))(step)
    .drop(10)
    .next()
    .score

  def solve2(lines: Seq[String]) =
    @annotation.tailrec
    def helper(grid: Grid, index: Int, prev: Seq[Grid]): (Int, Seq[Grid]) =
      val index = prev.lastIndexOf(grid)
      if index != -1 then (index, prev.drop(index))
      else helper(grid.step, index + 1, prev.appended(grid))

    val (offset, cycle) = helper(Parsing.parse(lines), 0, Seq())
    val relativeIndex   = ((1000000000L - offset) % cycle.size).toInt
    cycle(relativeIndex).score

object Test:
  lazy val file  = os.pwd / "2018" / "18" / "18.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)

object Main:
  lazy val file  = os.pwd / "2018" / "18" / "18.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 1147
  println(Main.res1) // part 1: 543312
  println(Main.res2) // part 2: 199064
