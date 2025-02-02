object DataDefs:
  enum Move:
    case U, D, L, R
  import Move.*

  type Code = Int | String
  type Pos  = (row: Int, col: Int, code: Code)

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
    yield Some((row, col, code))
  )

  def generateKeypad2: Keypad = // part 2
    val row1 = Seq(None, None, Some((0, 2, 1)), None, None)
    val row2 = Seq(None, Some((1, 1, 2)), Some((1, 2, 3)), Some((1, 3, 4)), None)
    val row3 = Seq(
      Some((2, 0, 5)),
      Some((2, 1, 6)),
      Some((2, 2, 7)),
      Some((2, 3, 8)),
      Some((2, 4, 9))
    )
    val row4 = Seq(
      None,
      Some((3, 1, "A")),
      Some((3, 2, "B")),
      Some((3, 3, "C")),
      None
    )
    val row5 = Seq(None, None, Some((4, 2, "D")), None, None)
    Keypad(Seq(row1, row2, row3, row4, row5))

  def parseMoves(lines: Seq[String]): List[List[Move]] =
    lines.map(line => line.map(_.toMove).toList).toList

object Solving:
  import DataDefs.*

  @annotation.tailrec
  private def traverseLine(start: Pos)(moves: List[Move])(keypad: Keypad): Pos =
    moves match
      case head :: next => traverseLine(keypad.go(start)(head))(next)(keypad)
      case Nil          => start

  @annotation.tailrec
  private def traverseLines(
      start: Pos,
      moves: List[List[Move]],
      acc: List[Pos]
  )(keypad: Keypad): List[Pos] =
    moves match
      case head :: next =>
        val end = traverseLine(start)(head)(keypad)
        traverseLines(end, next, end :: acc)(keypad)
      case Nil => acc.reverse

  def solve(lines: Seq[String])(start: (Int, Int, Int))(keypad: Keypad): String =
    traverseLines(start, Parsing.parseMoves(lines), Nil)(keypad).map(_.code).mkString

object Test:
  lazy val start1  = (1, 1, 5)
  lazy val start2  = (2, 0, 5)
  lazy val keypad1 = Parsing.generateKeypad1(3)
  lazy val keypad2 = Parsing.generateKeypad2
  lazy val file    = os.pwd / "2016" / "02" / "02.test.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val res1    = Solving.solve(lines)(start1)(keypad1)
  lazy val res2    = Solving.solve(lines)(start2)(keypad2)
// Test.res1 // part 1: 1985
// Test.res2 // part 2: 5DB3

object Main:
  lazy val start1  = (1, 1, 5)
  lazy val start2  = (2, 0, 5)
  lazy val keypad1 = Parsing.generateKeypad1(3)
  lazy val keypad2 = Parsing.generateKeypad2
  lazy val file    = os.pwd / "2016" / "02" / "02.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val res1    = Solving.solve(lines)(start1)(keypad1)
  lazy val res2    = Solving.solve(lines)(start2)(keypad2)
// Main.res1 // part 1: 78985
// Main.res2 // part 2: 57DD8
