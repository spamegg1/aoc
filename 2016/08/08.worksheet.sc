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
    private lazy val width  = rows.head.size
    private lazy val height = rows.size

    override def toString: String = rows // part 2, see output.txt
      .map(row => row.map(col => if col == On then "#" else ".").mkString)
      .mkString("\n")

    private def applyRect(c: Int, r: Int) =
      rows.zipWithIndex.map: (row, rowIndex) =>
        if 0 <= rowIndex && rowIndex < r then
          row.zipWithIndex.map: (col, colIndex) =>
            if 0 <= colIndex && colIndex < c then On else col
        else row

    private def applyRow(r: Int, shift: Int) =
      rows.zipWithIndex.map: (row, rowIndex) =>
        if rowIndex == r then
          row.zipWithIndex.map: (col, colIndex) =>
            row((colIndex - shift + width) % width)
        else row

    private def applyCol(c: Int, shift: Int) =
      rows.zipWithIndex.map: (row, rowIndex) =>
        row.zipWithIndex.map: (col, colIndex) =>
          if colIndex == c then rows((rowIndex - shift + height) % height)(colIndex)
          else col

    private def applyOp(op: Op) = Display:
      op match
        case Rect(c, r)    => applyRect(c, r)
        case Row(r, shift) => applyRow(r, shift)
        case Col(c, shift) => applyCol(c, shift)

    def applyOps(ops: Seq[Op]) = ops.foldLeft(this)((dis, op) => dis.applyOp(op))
    lazy val howManyOn         = rows.map(_.count(_ == On)).sum

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

  def solve(lines: Seq[String])(row: Int, col: Int) =
    val ops   = Parsing.parse(lines)
    val start = Display(row, col)
    start.applyOps(ops)

  def solve1(lines: Seq[String])(row: Int, col: Int) =
    solve(lines)(row, col).howManyOn

  def solve2(lines: Seq[String])(row: Int, col: Int) =
    val msg  = solve(lines)(row, col).toString
    val file = os.pwd / "2016" / "08" / "08.output.txt"
    os.write(file, msg)

object Test:
  lazy val file  = os.pwd / "2016" / "08" / "08.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve1(lines)(3, 7)
// Test.res // part 1: 6

object Main:
  lazy val file  = os.pwd / "2016" / "08" / "08.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(6, 50)
  lazy val res2  = Solving.solve2(lines)(6, 50)
// Main.res1 // part 1: 106
// Main.res2 // part 2: CFLELOYFCS
