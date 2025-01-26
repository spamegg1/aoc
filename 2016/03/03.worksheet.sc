object DataDefs:
  type Triangle = (a: Long, b: Long, c: Long)
  extension (t: Triangle)
    def isValid = t.a < t.b + t.c && t.b < t.a + t.c && t.c < t.a + t.b
  extension (t: Seq[Long]) def toTriangle = (t(0), t(1), t(2))

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Array[Long] = line
    .split(" ")
    .filter(_.nonEmpty)
    .map(_.toLong)

  def parseTriangle(line: String): Triangle        = parseLine(line).toSeq.toTriangle
  def parseRows(lines: Seq[String]): Seq[Triangle] = lines.map(parseTriangle)

  def parseCols(lines: Seq[String]): Seq[Triangle] =
    val parsedLines = lines.map(parseLine)
    val cols =
      for
        col   <- 0 until parsedLines(0).size
        array <- parsedLines
      yield array(col)
    cols
      .grouped(3)
      .map(_.toTriangle)
      .toSeq

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing.parseRows(lines).count(_.isValid)
  def solve2(lines: Seq[String]) = Parsing.parseCols(lines).count(_.isValid)

object Test:
  lazy val file  = os.pwd / "2016" / "03" / "03.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 3
// Test.res2 // part 2: 6

object Main:
  lazy val file  = os.pwd / "2016" / "03" / "03.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 983
// Main.res2 // part 2: 1836
