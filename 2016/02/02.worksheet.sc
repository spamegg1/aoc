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

  private val start1 = (1, 1, 5)
  private val start2 = (2, 0, 5)

  def solve1(lines: Seq[String])(using keypad: Keypad): String =
    val moves = Parsing.parseMoves(lines)
    traverseLines(start1, moves, Nil).map(_.code).mkString

  def solve2(lines: Seq[String])(using keypad: Keypad): String =
    val moves = Parsing.parseMoves(lines)
    traverseLines(start2, moves, Nil).map(_.code).mkString

object Test:
  import DataDefs.Keypad

  lazy val file  = os.pwd / "2016" / "02" / "02.test.input.txt"
  lazy val lines = os.read.lines(file)

  object Part1:
    given Keypad = Parsing.generateKeypad1(3) // part 1
    lazy val res = Solving.solve1(lines)

  object Part2:
    given Keypad = Parsing.generateKeypad2 // part 2
    lazy val res = Solving.solve2(lines)
// Test.Part1.res // part 1: 1985
// Test.Part2.res // part 2: 5DB3

object Main:
  import DataDefs.Keypad

  lazy val file  = os.pwd / "2016" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)

  object Part1:
    given Keypad = Parsing.generateKeypad1(3) // part 2
    lazy val res = Solving.solve1(lines)

  object Part2:
    given Keypad = Parsing.generateKeypad2 // part 2
    lazy val res = Solving.solve2(lines)
// Main.Part1.res // part 1: 78985
// Main.Part2.res // part 2: 57DD8
