/*
--- Day 20: Firewall Rules ---
You'd like to set up a small hidden computer here so you can
use it to get back into the network later.
However, the corporate firewall only allows communication with
certain external IP addresses.

You've retrieved the list of blocked IPs from the firewall,
but the list seems to be messy and poorly maintained,
and it's not clear which IPs are allowed.
Also, rather than being written in dot-decimal notation,
they are written as plain 32-bit integers,
which can have any value from 0 through 4294967295, inclusive.

For example, suppose only the values 0 through 9 were valid,
and that you retrieved the following blacklist:
5-8
0-2
4-7
The blacklist specifies ranges of IPs
(inclusive of both the start and end value)
that are not allowed.
Then, the only IPs that this firewall allows are 3 and 9,
since those are the only numbers not in any range.

Given the list of blocked IPs you retrieved from the firewall
(your puzzle input), what is the lowest-valued IP that is not blocked?

--- Part Two ---
How many IPs are allowed by the blacklist?
 */
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
    val ranges = Parsing.parse(lines)
    var (low, high) = ranges.head
    var lowest = 0L

    boundary:
      for (lo, hi) <- ranges.tail do
        if high + 1L < lo then
          lowest = high + 1L
          break()
        if lo < low then low = lo
        if high < hi then high = hi
    lowest

  def solve2(lines: Seq[String])(maximum: Long) =
    val ranges = Parsing.parse(lines)
    var (low, high) = ranges.head
    var total = low

    for (lo, hi) <- ranges.tail do
      if high < lo then
        total += lo - high - 1L
        low = lo
        high = hi
      else high = math.max(high, hi)
    total + maximum - high

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "20.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)(9L)
// Testing.result1 // part 1: 3
Testing.result2 // part 2: 2

object Main:
  lazy val lines = os.read.lines(os.pwd / "20.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)(DataDefs.max32)
// Main.result1 // part 1: 4793564
// Main.result2 // part 2: 146
