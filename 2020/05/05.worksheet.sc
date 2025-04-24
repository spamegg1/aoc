object DataDefs:
  case class Pass(row: Long, col: Long, seatId: Long)

object Parsing:
  import DataDefs.*, java.lang.Long.parseLong

  private val toBinary: Char => Char = { case 'F' | 'L' => '0'; case 'B' | 'R' => '1' }

  private def parseLine(rowSize: Int)(line: String): Pass =
    val (row, col) = (line.take(rowSize), line.drop(rowSize))
    val rowNum     = parseLong(row map toBinary, 2)
    val colNum     = parseLong(col map toBinary, 2)
    Pass(rowNum, colNum, rowNum * 8 + colNum)

  def parse(lines: Seq[String]) = lines map parseLine(7)

object Solving:
  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .maxBy(_.seatId)
    .seatId

  def solve2(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(_.seatId)
    .sorted
    .sliding(2)
    .find(seq => seq.head + 2 == seq.last)
    .map(_.head + 1)

object Test:
  lazy val file  = os.pwd / "2020" / "05" / "05.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines) // no testing data for part 2
// Test.res1 // part 1: 820

object Main:
  lazy val file  = os.pwd / "2020" / "05" / "05.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 888
// Main.res2 // part 2: 522
