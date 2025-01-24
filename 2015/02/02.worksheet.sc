object DataDefs:
  case class Dimensions(length: Long, width: Long, height: Long):
    lazy val side1 = length * width
    lazy val side2 = width * height
    lazy val side3 = height * length

    lazy val perim1 = 2 * (length + width)
    lazy val perim2 = 2 * (width + height)
    lazy val perim3 = 2 * (height + length)

    lazy val wrapping   = 2 * (side1 + side2 + side3)   // part 1
    lazy val slack      = List(side1, side2, side3).min // part 1
    lazy val totalPaper = wrapping + slack              // part 1

    lazy val volume      = length * width * height          // part 2
    lazy val perim       = List(perim1, perim2, perim3).min // part 2
    lazy val totalRibbon = volume + perim                   // part 2

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

object Test:
  lazy val file  = os.pwd / "2015" / "02" / "02.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve(lines)(_.totalPaper)
  lazy val res2  = Solving.solve(lines)(_.totalRibbon)
// Test.res1 // part1: 58 + 43 = 101
// Test.res2 // part2: 34 + 14 = 48

object Main:
  lazy val file  = os.pwd / "2015" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve(lines)(_.totalPaper)
  lazy val res2  = Solving.solve(lines)(_.totalRibbon)
// Main.res1 // part 1: 1586300
// Main.res2 // part 2: 3737498
