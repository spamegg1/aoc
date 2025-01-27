object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*

  def solve1(input: Long)(target: (Int, Int)) = 0L
  def solve2(input: Long)(target: (Int, Int)) = 0L

object Test:
  lazy val input  = 1358L
  lazy val target = (7, 4)
  lazy val res1   = Solving.solve1(input)(target)
  lazy val res2   = Solving.solve2(input)(target)
// Test.res1 // part 1: 11
// Test.res2 // part 2:

object Main:
  lazy val input  = 1358L
  lazy val target = (31, 39)
  lazy val res1   = Solving.solve1(input)(target)
  lazy val res2   = Solving.solve2(input)(target)
// Main.res1 // part 1:
// Main.res2 // part 2:
