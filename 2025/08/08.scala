package aoc2025.day08

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
  val file  = os.pwd / "2025" / "08" / "08.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2025" / "08" / "08.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 40
  println(Test.res2) // part 2:
  println(Main.res1) // part 1: 75680
  println(Main.res2) // part 2: 8995844880
