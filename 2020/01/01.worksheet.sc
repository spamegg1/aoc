object Parsing:
  def parse(lines: Seq[String]): Seq[Long] = lines.map(_.toLong)

object Solving:
  import util.boundary, boundary.break

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
    val numbers         = Parsing.parse(lines)
    val (first, second) = findPair(numbers)
    first * second

  def solve2(lines: Seq[String]): Long =
    val numbers                = Parsing.parse(lines)
    val (first, second, third) = findTriple(numbers)
    first * second * third

object Test:
  lazy val file  = os.pwd / "2020" / "01" / "01.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 514579
// Test.res2 // part 2: 241861950

object Main:
  lazy val file  = os.pwd / "2020" / "01" / "01.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 290784
// Main.res2 // part 2: 177337980
