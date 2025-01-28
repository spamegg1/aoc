import scala.util.boundary, boundary.break

object DataDefs:
  val max32 = 4294967295L

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line match
    case s"$lo-$hi" => (lo.toLong, hi.toLong)

  def parse(lines: Seq[String]) = lines.map(parseLine).sortBy(_._1)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val ranges      = Parsing.parse(lines)
    var (low, high) = ranges.head
    var lowest      = 0L

    boundary:
      for (lo, hi) <- ranges.tail do
        if high + 1L < lo then
          lowest = high + 1L
          break()
        if lo < low then low = lo
        if high < hi then high = hi
    lowest

  def solve2(lines: Seq[String])(maximum: Long) =
    val ranges      = Parsing.parse(lines)
    var (low, high) = ranges.head
    var total       = low

    for (lo, hi) <- ranges.tail do
      if high < lo then
        total += lo - high - 1L
        low = lo
        high = hi
      else high = math.max(high, hi)
    total + maximum - high

object Test:
  lazy val file  = os.pwd / "2016" / "20" / "20.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(9L)
// Test.res1 // part 1: 3
// Test.res2 // part 2: 2

object Main:
  lazy val file  = os.pwd / "2016" / "20" / "20.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(DataDefs.max32)
// Main.res1 // part 1: 4793564
// Main.res2 // part 2: 146
