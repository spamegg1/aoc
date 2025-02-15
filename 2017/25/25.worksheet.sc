object DataDefs:
  ???

object Parsing:
  import DataDefs.*

  def parseLine(line: String)   = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*
  def solve(lines: Seq[String]) = 0L

object Test:
  lazy val file  = os.pwd / "2017" / "25" / "25.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
// Test.res // part 1:

object Main:
  lazy val file  = os.pwd / "2017" / "25" / "25.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
// Main.res // part 1:
