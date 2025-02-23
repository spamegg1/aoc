package aoc2018.day25

object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = ???

object Solving:
  import DataDefs.*
  def solve(lines: Seq[String]) = 0L

object Test:
  lazy val file1 = os.pwd / "2018" / "25" / "25.test.input.1.txt"
  lazy val file2 = os.pwd / "2018" / "25" / "25.test.input.2.txt"
  lazy val file3 = os.pwd / "2018" / "25" / "25.test.input.3.txt"
  lazy val files = Seq(file1, file2, file3)
  lazy val lines = files map os.read.lines
  lazy val res   = lines map Solving.solve

object Main:
  lazy val file  = os.pwd / "2018" / "25" / "25.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

@main
def run: Unit =
  println(Test.res) // part 1: 4,3,8
  println(Main.res) // part 1: 381
