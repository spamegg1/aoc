/*
--- Day 1: Report Repair ---
After saving Christmas five years in a row, you've decided to take a vacation at
a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold
coins used there have a little picture of a starfish; the locals just call them
stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay
the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense
report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then
  multiply those two numbers together.
For example, suppose your expense report contained the following:
1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?

--- Part Two ---
The Elves in accounting are thankful for your help; one of them even offers you
a starfish coin they had left over from a past vacation. They offer you a second
one if you can find three numbers in your expense report that meet the same
criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366,
and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to
2020?
 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Seq[Long] = lines.map(_.toLong)

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def findPair(numbers: Seq[Long]): (Long, Long) =
    var (first, second) = (0L, 0L)
    boundary:
      for
        num1 <- numbers
        num2 <- numbers
        if num1 + num2 == 2020
      do
        first = num1
        second = num2
        break()
    (first, second)

  def findTriple(numbers: Seq[Long]): (Long, Long, Long) =
    var (first, second, third) = (0L, 0L, 0L)
    boundary:
      for
        num1 <- numbers
        num2 <- numbers
        num3 <- numbers
        if num1 + num2 + num3 == 2020
      do
        first = num1
        second = num2
        third = num3
        break()
    (first, second, third)

  def solve1(lines: Seq[String]): Long =
    val numbers = Parsing.parse(lines)
    val (first, second) = findPair(numbers)
    first * second

  def solve2(lines: Seq[String]): Long =
    val numbers = Parsing.parse(lines)
    val (first, second, third) = findTriple(numbers)
    first * second * third

object Testing:
  lazy val input =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin.split("\n").toSeq
  lazy val result1 = Solving.solve1(input)
  lazy val result2 = Solving.solve2(input)
Testing.result1 // part 1: 514579
Testing.result2 // part 2: 241861950

object Main:
  lazy val lines = os.read.lines(os.pwd / "01.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 290784
Main.result2 // part 2: 177337980
