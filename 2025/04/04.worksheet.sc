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

object Test:
  private lazy val lines = os.read.lines(os.pwd / "01.test.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
Test.res1 // part 1:
Test.res2 // part 2:

object Main:
  private lazy val lines = os.read.lines(os.pwd / "01.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
Main.res1 // part 1:
Main.res2 // part 2:
