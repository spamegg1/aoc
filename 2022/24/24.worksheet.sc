object DataDefs:
  val Moves = Seq(Point(0, 0), Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)
    def neighbours: Seq[Point] = Moves.map(_ + this)

object Parsing:
  import DataDefs.*

  def mod(a: Int, m: Int): Int =
    val remainder = (a - 1) % m
    if remainder < 0 then remainder + m + 1 else remainder + 1

  def valid(lines: Seq[String])(w: Int, h: Int)(time: Int)(p: Point): Boolean =
    val Point(x, y) = p
    lines.indices.contains(y) &&
    lines(y)(x) != '#' &&
    lines(y)(mod(x + time, w)) != '<' &&
    lines(y)(mod(x - time, w)) != '>' &&
    lines(mod(y + time, h))(x) != '^' &&
    lines(mod(y - time, h))(x) != 'v'

  def sim(lines: Seq[String])(w: Int, h: Int)(ps: Set[Point], end: Point, t: Int): Int =
    if ps.contains(end) then t
    else
      val next = ps.flatMap(_.neighbours).filter(valid(lines)(w, h)(t + 1))
      sim(lines)(w, h)(next, end, t + 1)

  def parse(lines: Seq[String]) =
    val width  = lines.head.size - 2
    val height = lines.size - 2
    (Point(1, 0), Point(width, height + 1), sim(lines)(width, height))

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val (start, end, simulate) = Parsing.parse(lines)
    simulate(Set(start), end, 0)

  def solve2(lines: Seq[String]) =
    val (start, end, simulate) = Parsing.parse(lines)
    val time1                  = simulate(Set(start), end, 0)
    val time2                  = simulate(Set(end), start, time1)
    simulate(Set(start), end, time2)

object Test:
  val file  = os.pwd / "2022" / "24" / "24.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 18
// Test.res2 // part 2: 54

object Main:
  val file  = os.pwd / "2022" / "24" / "24.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 343
// Main.res2 // part 2: 960
