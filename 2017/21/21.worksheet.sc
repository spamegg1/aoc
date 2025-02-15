object DataDefs:
  ???

object Parsing:
  import DataDefs.*

  def parseLine(line: String)   = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Test:
  lazy val file  = os.pwd / "2017" / "21" / "21.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1:
// Test.res2 // part 2:

object Main:
  lazy val file  = os.pwd / "2017" / "21" / "21.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 133
// Main.res2 // part 2: 2221990
