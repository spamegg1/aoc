/*
--- Day 3: Squares With Three Sides ---
Now that you can think clearly, you move deeper into the labyrinth of hallways
and office furniture that makes up this part of Easter Bunny HQ. This must be a
graphic design department; the walls are covered in specifications for triangles.

Or are they?

The design document gives the side lengths of each triangle it describes, but...
5 10 25? Some of these aren't triangles. You can't help but mark the impossible
ones.

In a valid triangle, the sum of any two sides must be larger than the remaining
side. For example, the "triangle" given above is impossible, because 5 + 10 is
not larger than 25.

In your puzzle input, how many of the listed triangles are possible?

--- Part Two ---
Now that you've helpfully marked up their design documents, it occurs to you
that triangles are specified in groups of three vertically. Each set of three
numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds
digit would be part of the same triangle:

101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603

In your puzzle input, and instead reading by columns, how many of the listed
triangles are possible?
 */
object DataDefs:
  case class Triangle(a: Long, b: Long, c: Long):
    lazy val isValid = a < b + c && b < a + c && c < a + b

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Array[Long] =
    line.split(" ").filter(_.nonEmpty).map(_.toLong)

  def parseTriangle(line: String): Triangle =
    val sides = parseLine(line)
    Triangle(sides(0), sides(1), sides(2))

  def parseRows(lines: Seq[String]): Seq[Triangle] =
    lines.map(parseTriangle)

  def parseCols(lines: Seq[String]): Seq[Triangle] =
    val parsedLines = lines.map(parseLine)
    val cols =
      for
        col <- 0 until parsedLines(0).size
        array <- parsedLines
      yield array(col)
    cols
      .grouped(3)
      .map(sides => Triangle(sides(0), sides(1), sides(2)))
      .toSeq

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing.parseRows(lines).count(_.isValid)
  def solve2(lines: Seq[String]) = Parsing.parseCols(lines).count(_.isValid)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "03.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 3
Testing.result2 // part 2: 6

object Main:
  private lazy val lines = os.read.lines(os.pwd / "03.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 983
Main.result2 // part 2: 1836
