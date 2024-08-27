/*
--- Day 8: Two-Factor Authentication ---
You come across a door implementing what you can only assume is an implementation
 of two-factor authentication after a long game of requirements telephone.

To get past the door, you first swipe a keycard
(no problem; there was one on a nearby desk).
Then, it displays a code on a little screen, and you type that code on a keypad.
Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed.
After a few minutes, you've taken everything apart and figured out how it works.
Now you just have to work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for the screen;
these instructions are your puzzle input.
The screen is 50 pixels wide and 6 pixels tall, all of which start off,
and is capable of three somewhat peculiar operations:
  rect AxB turns on all of the pixels in a rectangle at the top-left of the screen
    which is A wide and B tall.
  rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B
    pixels. Pixels that would fall off the right end appear at the left end of the row.
  rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down
    by B pixels. Pixels that would fall off the bottom appear at the top of the column.

For example, here is a simple sequence on a smaller screen:
  rect 3x2 creates a small rectangle in the top-left corner:
    ###....
    ###....
    .......
  rotate column x=1 by 1 rotates the second column down by one pixel:
    #.#....
    ###....
    .#.....
  rotate row y=0 by 4 rotates the top row right by four pixels:
    ....#.#
    ###....
    .#.....
  rotate column x=1 by 1 again rotates the second column down by one pixel,
    causing the bottom pixel to wrap back to the top:
    .#..#.#
    #.#....
    .#.....

As you can see, this display technology is extremely powerful,
and will soon dominate the tiny-code-displaying-screen market.
That's what the advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display:
after you swipe your card, if the screen did work, how many pixels should be lit?

--- Part Two ---
You notice that the screen is only capable of displaying capital letters;
in the font it uses, each letter is 5 pixels wide and 6 tall.
After you swipe your card, what code is the screen trying to display?
 */
object DataDefs:
  enum Op:
    case Rect(col: Int, row: Int)
    case Row(row: Int, shift: Int)
    case Col(col: Int, shift: Int)
  import Op.*

  enum Pixel:
    case On, Off
  import Pixel.*

  case class Display(rows: Seq[Seq[Pixel]]):
    private lazy val width = rows.head.size
    private lazy val height = rows.size

    override def toString: String = rows
      .map(row => row.map(col => if col == On then "#" else ".").mkString)
      .mkString("\n")

    private def applyRect(c: Int, r: Int) = rows.zipWithIndex.map: (row, rowIndex) =>
      if 0 <= rowIndex && rowIndex < r then
        row.zipWithIndex.map: (col, colIndex) =>
          if 0 <= colIndex && colIndex < c then On else col
      else row

    private def applyRow(r: Int, shift: Int) = rows.zipWithIndex.map: (row, rowIndex) =>
      if rowIndex == r then
        row.zipWithIndex.map: (col, colIndex) =>
          row((colIndex - shift + width) % width)
      else row

    private def applyCol(c: Int, shift: Int) = rows.zipWithIndex.map: (row, rowIndex) =>
      row.zipWithIndex.map: (col, colIndex) =>
        if colIndex == c then rows((rowIndex - shift + height) % height)(colIndex)
        else col

    private def applyOp(op: Op): Display = Display(op match
      case Rect(c, r)    => applyRect(c, r)
      case Row(r, shift) => applyRow(r, shift)
      case Col(c, shift) => applyCol(c, shift)
    )

    def applyOps(ops: Seq[Op]) = ops.foldLeft(this)((display, op) => display.applyOp(op))
    lazy val howManyOn = rows.map(_.count(_ == On)).sum

  object Display:
    def apply(row: Int, col: Int) =
      new Display((0 until row).map(_ => (0 until col).map(_ => Off)))

object Parsing:
  import DataDefs.*, Op.*

  private def parseLine(line: String): Op = line match
    case s"rect ${col}x$row"               => Rect(col.toInt, row.toInt)
    case s"rotate row y=$row by $shift"    => Row(row.toInt, shift.toInt)
    case s"rotate column x=$col by $shift" => Col(col.toInt, shift.toInt)

  def parse(lines: Seq[String]): Seq[Op] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(row: Int, col: Int) =
    val ops = Parsing.parse(lines)
    val start = Display(row, col)
    start.applyOps(ops) // .howManyOn

  def solve2(lines: Seq[String])(row: Int, col: Int) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "08" / "08.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(3, 7)
  lazy val result2 = Solving.solve2(lines)(3, 7)
// Testing.result1 // part 1: 6
// Testing.result2 // part 2:

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "08" / "08.input.txt")
  lazy val result1 = Solving.solve1(lines)(6, 50)
  lazy val result2 = Solving.solve2(lines)(6, 50)
// Main.result1 // part 1: 106
// Main.result2 // part 2:
