/*
--- Day 2: I Was Told There Would Be No Math ---
The elves are running low on wrapping paper, and so they need to submit an order
for more. They have a list of the dimensions (length l, width w, and height h)
of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which
makes calculating the required wrapping paper for each gift a little easier:
    find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves
    also need a little extra paper for each present: the area of the smallest
    side.

For example:
  A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet
  of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
  A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet
  of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.

All numbers in the elves' list are in feet. How many total square feet of
wrapping paper should they order?

--- Part Two ---
The elves are also running low on ribbon. Ribbon is all the same width, so they
only have to worry about the length they need to order, which they would again
like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides,
or the smallest perimeter of any one face. Each present also requires a bow made
out of ribbon as well; the feet of ribbon required for the perfect bow is equal
to the cubic feet of volume of the present. Don't ask how they tie the bow,
though; they'll never tell.

For example:
A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap
the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap
the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.

How many total feet of ribbon should they order?
 */
object DataDefs:
  case class Dimensions(length: Long, width: Long, height: Long):
    lazy val side1 = length * width
    lazy val side2 = width * height
    lazy val side3 = height * length

    lazy val perim1 = 2 * (length + width)
    lazy val perim2 = 2 * (width + height)
    lazy val perim3 = 2 * (height + length)

    lazy val wrapping = 2 * (side1 + side2 + side3) // part 1
    lazy val slack = List(side1, side2, side3).min // part 1
    lazy val totalPaper = wrapping + slack // part 1

    lazy val volume = length * width * height // part 2
    lazy val perim = List(perim1, perim2, perim3).min // part 2
    lazy val totalRibbon = volume + perim // part 2

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Dimensions = line match
    case s"${length}x${width}x${height}" =>
      Dimensions(length.toLong, width.toLong, height.toLong)

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String])(fun: Dimensions => Long): Long = lines.view
    .map(Parsing.parseLine)
    .map(fun)
    .sum

object Testing:
  private lazy val file = os.pwd / "2015" / "02" / "02.test.input.txt"
  private lazy val lines = os.read.lines(file)
  lazy val result1 = Solving.solve(lines)(_.totalPaper)
  lazy val result2 = Solving.solve(lines)(_.totalRibbon)
// Testing.result1 // part1: 58 + 43 = 101
// Testing.result2 // part2: 34 + 14 = 48

object Main:
  private lazy val file = os.pwd / "2015" / "02" / "02.input.txt"
  private lazy val lines = os.read.lines(file)
  lazy val result1 = Solving.solve(lines)(_.totalPaper)
  lazy val result2 = Solving.solve(lines)(_.totalRibbon)
// Main.result1 // part 1: 1586300
// Main.result2 // part 2: 3737498
