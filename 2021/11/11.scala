package aoc2021.day11

object DataDefs:
  val MaxEnergy = 9
  val Bound = Seq((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))

  type Octo = Int
  type Cave = Map[Pos, Octo]

  extension (cave: Cave)
    def neighbors(p: Pos) = Bound.map(p.delta).filter(cave.contains)
    def flashes           = cave.values.count(_ == 0)

  case class Pos(row: Int, col: Int):
    def delta(r: Int, c: Int) = Pos(r + row, c + col)

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Cave = Seq
    .tabulate(lines.size, lines.head.size): (row, col) =>
      Pos(row, col) -> lines(row)(col).asDigit
    .flatten
    .toMap

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def helper(cave: Cave, todo: Seq[Pos], flashed: Set[Pos]): Cave = todo match
    case Nil => cave
    case head :: tail =>
      if flashed.contains(head) then helper(cave, tail, flashed)
      else if cave(head) < MaxEnergy then
        val newCave = cave.updated(head, cave(head) + 1)
        helper(newCave, tail, flashed)
      else
        val newCave = cave.updated(head, 0)
        val newTodo = tail ++ cave.neighbors(head)
        helper(newCave, newTodo, flashed + head)

  def step(cave: Cave): Cave = helper(cave, cave.keys.toSeq, Set())

  def solve1(lines: Seq[String]) =
    val cave = Parsing.parse(lines)
    Iterator
      .iterate(cave)(step)
      .take(101)
      .map(_.flashes)
      .sum

  def solve2(lines: Seq[String]) =
    val cave = Parsing.parse(lines)
    Iterator
      .iterate(cave)(step)
      .indexWhere(_.flashes == 100)

object Test:
  val file1  = os.pwd / "2021" / "11" / "11.test.input.1.txt"
  val file2  = os.pwd / "2021" / "11" / "11.test.input.2.txt"
  val lines1 = os.read.lines(file1)
  val lines2 = os.read.lines(file2)
  val res11  = Solving.solve1(lines1)
  val res12  = Solving.solve1(lines2)

object Main:
  val file  = os.pwd / "2021" / "11" / "11.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res11) // part 1: 259
  println(Test.res12) // part 1: 1656
  println(Main.res1)  // part 1: 1729
  println(Main.res2)  // part 2: 237
