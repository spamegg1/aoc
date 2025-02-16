package aoc2017.day22

object DataDefs:
  enum Node:
    case Clean, Weakened, Infected, Flagged
  import Node.*

  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def +(that: Pos) = (x = p.x + that.x, y = p.y + that.y)
    def clockWise    = (-p.y, p.x)
    def counterClock = (p.y, -p.x)

  case class State(grid: Map[Pos, Node], pos: Pos, dir: Pos, infected: Int):
    def next(node: Node, newDir: Pos, infect: Int): State =
      State(grid.updated(pos, node), pos + newDir, newDir, infected + infect)

    def step1 = grid(pos) match
      case Clean    => next(Infected, dir.counterClock, 1)
      case Infected => next(Clean, dir.clockWise, 0)
      case _        => throw MatchError("Unreachable")

    def step2 = grid(pos) match
      case Clean    => next(Weakened, dir.counterClock, 0)
      case Weakened => next(Infected, dir, 1)
      case Infected => next(Flagged, dir.clockWise, 0)
      case Flagged  => next(Clean, dir.clockWise.clockWise, 0)

object Parsing:
  import DataDefs.*, Node.*

  def parse(lines: Seq[String]) =
    val poses: Seq[Pos] =
      for
        x <- lines.indices
        y <- lines.indices
        if lines(y)(x) == '#'
      yield (x = x, y = y)
    val grid = poses.map(_ -> Infected).toMap.withDefaultValue(Clean)
    State(grid, (x = lines.size / 2, y = lines.size / 2), (x = 0, y = -1), 0)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val state = Parsing.parse(lines)
    Iterator.iterate(state)(_.step1).drop(10000).next().infected

  def solve2(lines: Seq[String]) =
    val state = Parsing.parse(lines)
    Iterator.iterate(state)(_.step2).drop(10000000).next().infected

object Test:
  lazy val file  = os.pwd / "2017" / "22" / "22.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2017" / "22" / "22.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 5587
  println(Test.res2) // part 2: 2511944
  println(Main.res1) // part 1: 5462
  println(Main.res2) // part 2: 2512135
