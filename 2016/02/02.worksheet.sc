/*
--- Day 2: Bathroom Security ---
You arrive at Easter Bunny Headquarters under cover of darkness. However, you
left in such a rush that you forgot to use the bathroom! Fancy office buildings
like this one usually have keypad locks on their bathrooms, so you search the
front desk for the code.

"In order to improve security," the document you find says, "bathroom codes will
no longer be written down. Instead, please memorize and follow the procedure
below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by
starting on the previous button and moving to adjacent buttons on the keypad: U
moves up, D moves down, L moves left, and R moves right. Each line of
instructions corresponds to one button, starting at the previous button (or, for
the first line, the "5" button); press whatever button you're on at the end of
each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk
to the bathroom. You picture a keypad like this:
1 2 3
4 5 6
7 8 9
Suppose your instructions are:
ULL
RRDDD
LURDL
UUUUD
You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and
  stay on "1"), so the first button is 1.
Starting from the previous button ("1"), you move right twice (to "3") and
then down three times (stopping at "9" after two moves and ignoring the
  third), ending up with 9.
Continuing from "9", you move left, up, right, down, and left, ending with 8.
Finally, you move up four times (stopping at "2"), then down once, ending with 5.

So, in this example, the bathroom code is 1985.
Your puzzle input is the instructions from the document you found at the front
desk. What is the bathroom code?

--- Part Two ---
You finally arrive at the bathroom (it's a several minute walk from the lobby so
visitors can behold the many fancy conference rooms and water coolers on this
floor) and go to punch in the code. Much to your bladder's dismay, the keypad is
not at all like you imagined it. Instead, you are confronted with the result of
hundreds of man-hours of bathroom-keypad-design meetings:
    1
  2 3 4
5 6 7 8 9
  A B C
    D
You still start at "5" and stop when you're at an edge, but given the same
instructions as above, the outcome is very different:
You start at "5" and don't move at all (up and left are both edges), ending at
5.
Continuing from "5", you move right twice and down three times (through "6",
"7", "B", "D", "D"), ending at D.
Then, from "D", you move five more times (through "D", "B", "C", "C", "B"),
ending at B.
Finally, after five more moves, you end at 3.

So, given the actual keypad layout, the code would be 5DB3.
Using the same instructions in your puzzle input, what is the correct bathroom
code?
 */
object DataDefs:
  enum Move:
    case U, D, L, R
  import Move.*

  type Code = Int | String
  case class Pos(row: Int, col: Int, code: Code)

  case class Keypad(grid: Seq[Seq[Option[Pos]]]):
    val rows = grid.size
    val cols = grid(0).size

    def go(pos: Pos)(move: Move) = move match
      case U => if pos.row == 0 then pos else grid(pos.row - 1)(pos.col).getOrElse(pos)
      case D =>
        if pos.row == rows - 1 then pos else grid(pos.row + 1)(pos.col).getOrElse(pos)
      case L => if pos.col == 0 then pos else grid(pos.row)(pos.col - 1).getOrElse(pos)
      case R =>
        if pos.col == cols - 1 then pos else grid(pos.row)(pos.col + 1).getOrElse(pos)

object Parsing:
  import DataDefs.*, Move.*

  extension (char: Char)
    def toMove: Move = char match
      case 'U' => U
      case 'D' => D
      case 'L' => L
      case 'R' => R

  def generateKeypad1(size: Int): Keypad = Keypad( // part 1
    for row <- 0 until size
    yield for
      col <- 0 until size
      code = row * size + col + 1
    yield Some(Pos(row, col, code))
  )

  def generateKeypad2: Keypad = // part 2
    val row1 = Seq(None, None, Some(Pos(0, 2, 1)), None, None)
    val row2 = Seq(None, Some(Pos(1, 1, 2)), Some(Pos(1, 2, 3)), Some(Pos(1, 3, 4)), None)
    val row3 = Seq(
      Some(Pos(2, 0, 5)),
      Some(Pos(2, 1, 6)),
      Some(Pos(2, 2, 7)),
      Some(Pos(2, 3, 8)),
      Some(Pos(2, 4, 9))
    )
    val row4 = Seq(
      None, 
      Some(Pos(3, 1, "A")), 
      Some(Pos(3, 2, "B")), 
      Some(Pos(3, 3, "C")), 
      None
    )
    val row5 = Seq(None, None, Some(Pos(4, 2, "D")), None, None)
    Keypad(Seq(row1, row2, row3, row4, row5))

  def parseMoves(lines: Seq[String]): List[List[Move]] =
    lines.map(line => line.map(_.toMove).toList).toList

object Solving:
  import DataDefs.*

  @annotation.tailrec
  private def traverseLine(start: Pos)(moves: List[Move])(using keypad: Keypad): Pos =
    moves match
      case head :: next => traverseLine(keypad.go(start)(head))(next)
      case Nil          => start

  @annotation.tailrec
  private def traverseLines(
      start: Pos,
      moves: List[List[Move]],
      acc: List[Pos]
  )(using keypad: Keypad): List[Pos] =
    moves match
      case head :: next =>
        val end = traverseLine(start)(head)
        traverseLines(end, next, end :: acc)
      case Nil => acc.reverse

  private val start1 = Pos(1, 1, 5)
  private val start2 = Pos(2, 0, 5)

  def solve1(lines: Seq[String])(using keypad: Keypad): String =
    val moves = Parsing.parseMoves(lines)
    traverseLines(start1, moves, Nil).map(_.code).mkString

  def solve2(lines: Seq[String])(using keypad: Keypad): String =
    val moves = Parsing.parseMoves(lines)
    traverseLines(start2, moves, Nil).map(_.code).mkString

object Testing:
  import DataDefs.Keypad
  private lazy val lines = os.read.lines(os.pwd / "2016" / "02" / "02.test.input.txt")
  object Part1:
    given Keypad = Parsing.generateKeypad1(3) // part 1
    lazy val result = Solving.solve1(lines)
  object Part2:
    given Keypad = Parsing.generateKeypad2 // part 2
    lazy val result = Solving.solve2(lines)
// Testing.Part1.result // part 1: 1985
// Testing.Part2.result // part 2: 5DB3

object Main:
  import DataDefs.Keypad
  private lazy val lines = os.read.lines(os.pwd / "2016" / "02" / "02.input.txt")
  object Part1:
    given Keypad = Parsing.generateKeypad1(3) // part 2
    lazy val result = Solving.solve1(lines)
  object Part2:
    given Keypad = Parsing.generateKeypad2 // part 2
    lazy val result = Solving.solve2(lines)
// Main.Part1.result // part 1: 78985
// Main.Part2.result // part 2: 57DD8
