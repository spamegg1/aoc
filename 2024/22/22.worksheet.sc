/*

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Testing:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "22" / "22.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1:
// Testing.result2 // part 2:

object Main:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "22" / "22.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1:
// Main.result2 // part 2:
