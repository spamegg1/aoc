object Parsing:
  def parse(lines: Seq[String]) = lines.head
    .split(",")
    .map(_.toInt)
    .toSeq

object Solving:
  private def solve(lines: Seq[String])(dist: (Int, Int) => Int) =
    val positions  = Parsing.parse(lines)
    val (min, max) = (positions.min, positions.max)
    (min to max)
      .map(pos => positions.map(crab => dist(crab, pos)).sum)
      .min

  def solve1(lines: Seq[String]) = solve(lines)((p1, p2) => math.abs(p1 - p2))

  private def cumulativeDistance(p1: Int, p2: Int): Int =
    val steps = math.abs(p1 - p2)
    steps * (steps + 1) / 2
  def solve2(lines: Seq[String]) = solve(lines)(cumulativeDistance)

object Test:
  val file  = os.pwd / "2021" / "07" / "07.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 37
// Test.res2 // part 2: 168

object Main:
  val file  = os.pwd / "2021" / "07" / "07.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 348996
// Main.res2 // part 2: 98231647
