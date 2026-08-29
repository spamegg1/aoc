object DataDefs:
  case class Point(x: Int, y: Int)
  case class Info(sensor: Point, beacon: Point, manhattan: Int)

  case class Interval(min: Int, max: Int):
    def size: Int                    = max - min + 1
    infix def merge(other: Interval) = Interval(min.min(other.min), max.max(other.max))
    def touching(other: Interval): Boolean =
      if other.min < min then min - other.max <= 1 else other.min - max <= 1

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Info =
    val Array(x1, y1, x2, y2) = line
      .split("[^-\\d]+")
      .tail
      .map(_.toInt)
    Info(Point(x1, y1), Point(x2, y2), (x1 - x2).abs + (y1 - y2).abs)

  def parse(lines: Seq[String]): Seq[Info] = lines.map(parseLine)

object Solving:
  import DataDefs.*

  def build(info: Seq[Info], y: Int): Seq[Interval] = info
    .foldLeft(Seq.empty[Interval]):
      case (intervals, info) =>
        val extra = info.manhattan - (info.sensor.y - y).abs
        if extra < 0 then intervals
        else
          val next      = Interval(info.sensor.x - extra, info.sensor.x + extra)
          val (in, out) = intervals.partition(_.touching(next))
          in.foldLeft(next)(_ merge _) +: out

  def solve1(lines: Seq[String])(row: Int): Int =
    val info      = Parsing.parse(lines)
    val buildSize = build(info, row).head.size
    val infoSize = info
      .map(_.beacon)
      .filter(_.y == row)
      .distinct
      .size
    buildSize - infoSize

  @annotation.tailrec
  def helper(info: Seq[Info], y: Int): Long = build(info, y) match
    case Seq(first, second) => 4000000L * (first.max.min(second.max) + 1) + y
    case _                  => helper(info, y + 1)

  def solve2(lines: Seq[String]) = helper(Parsing.parse(lines), 0)

object Test:
  val file  = os.pwd / "2022" / "15" / "15.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(10)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 26
// Test.res2 // part 2: 56000011

object Main:
  val file  = os.pwd / "2022" / "15" / "15.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(2000000)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 4424278
// Main.res2 // part 2: 10382630753392
