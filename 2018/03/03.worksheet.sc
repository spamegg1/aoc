/*
--- Day 3: No Matter How You Slice It ---
The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit
(thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in
the middle of the night). Unfortunately, anomalies are still affecting them -
nobody can even agree on how to cut the fabric.

The whole piece of fabric they're working on is a very large square -
at least 1000 inches on each side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's suit.
All claims have an ID and consist of a single rectangle with edges parallel to the edges
of the fabric. Each claim's rectangle is defined as follows:
  The number of inches between left edge of fabric and left edge of rectangle.
  The number of inches between top edge of fabric and edge of rectangle.
  The width of the rectangle in inches.
  The height of the rectangle in inches.

A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle
3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.
Visually, it claims the square inches of fabric represented by #
(and ignores the square inches of fabric represented by .) in the diagram below:
...........
...........
...#####...
...#####...
...#####...
...#####...
...........
...........
...........

The problem is that many of the claims overlap, causing two or more claims
to cover part of the same areas. For example, consider the following claims:

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2

Visually, these claim the following areas:

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........

The four square inches marked with X are claimed by both 1 and 2.
(Claim 3, while adjacent to the others, does not overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough fabric.
How many square inches of fabric are within two or more claims?

--- Part Two ---
Amidst the chaos, you notice that exactly one claim doesn't overlap
by even a single square inch of fabric with any other claim.
If you can somehow draw attention to it, maybe the Elves
will be able to make Santa's suit after all!

For example, in the claims above, only claim 3
is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?

 */
object DataDefs:
  case class Claim(id: Int, rows: Range, cols: Range):
    def intersect(that: Claim) =
      for
        row <- rows.intersect(that.rows)
        col <- cols.intersect(that.cols)
      yield (row, col)

object Parsing:
  import DataDefs.*

  private def parseLine(line: String) = line match
    case s"#$id @ $row,$col: ${rows}x$cols" =>
      Claim(
        id.toInt,
        Range(row.toInt, row.toInt + rows.toInt),
        Range(col.toInt, col.toInt + cols.toInt)
      )

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .combinations(2)
    .flatMap { case Seq(c1, c2) => c1.intersect(c2) }
    .distinct
    .size

  def solve2(lines: Seq[String]) =
    val claims = Parsing.parse(lines)
    claims
      .find: c1 =>
        claims.forall(c2 => c1 == c2 || c1.intersect(c2).isEmpty)
      .get
      .id

object Testing:
  lazy val lines = os.read.lines(os.pwd / "2018" / "03" / "03.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 4
// Testing.result2 // part 2: 3

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "03" / "03.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 110389
// Main.result2 // part 2: 552
