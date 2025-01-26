package aoc2015.day24

object DataDefs:
  val MAX = 6 // I will only consider subsets up to this size

object Parsing:
  def parse(lines: Seq[String]) = lines.map(_.toLong)

object Solving:
  import DataDefs.*

  def solve(factor: Long)(lines: Seq[String]) =
    val nums  = Parsing.parse(lines)
    val third = nums.sum / factor
    (1 to MAX) // get all subsets of size 1, ..., MAX
      .flatMap: size =>
        nums                      // Seq[Long]
          .combinations(size)     // Iterator[Seq[Long]]
          .filter(_.sum == third) // Iterator[Seq[Long]]
          .toSeq                  // Seq[Seq[Long]]
      .groupBy(_.size)            // Map[Int, Seq[Seq[Long]]]
      .minBy(_._1)                // (Int, Seq[Seq[Long]])
      ._2                         // Seq[Seq[Long]]
      .minBy(_.product)           // Seq[Long]
      .product                    // Long

  val solve1 = solve(3L)
  val solve2 = solve(4L)

object Test:
  lazy val file  = os.pwd / "2015" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2015" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 99
  println(Test.res2) // part 2: 44
  println(Main.res1) // part 1: 10439961859
  println(Main.res2) // part 2: 72050269
