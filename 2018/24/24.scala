package aoc2018.day24

object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = ???

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Test:
  lazy val file  = os.pwd / "2018" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2018" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 5216
  println(Test.res2) // part 2: 51
  println(Main.res1) // part 1: 21070
  println(Main.res2) // part 2: 7500
