object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parseLine(line: String)   = 0
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Test: // target area: x=20..30, y=-10..-5
  val res1 = Solving.solve1(Nil)
  val res2 = Solving.solve2(Nil)
// Test.res1 // part 1:
// Test.res2 // part 2:

object Main: // target area: x=195..238, y=-93..-67
  val res1 = Solving.solve1(Nil)
  val res2 = Solving.solve2(Nil)
// Main.res1 // part 1:
// Main.res2 // part 2:
