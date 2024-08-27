/*

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = ???

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "07" / "07.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: ???
// Testing.result2 // part 2: ???

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "07" / "07.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: ???
// Main.result2 // part 2: ???
