object DataDefs:
  enum Light:
    case On, Off

    def next(neighbors: Seq[Light]) =
      val on = neighbors.count(_ == On)
      this match
        case On  => if on == 2 || on == 3 then On else Off
        case Off => if on == 3 then On else Off
  import Light.*

  extension (c: Char)
    def toLight = c match
      case '#' => On
      case '.' => Off

  extension (pos: (Int, Int))
    def neighbors =
      val (r, c) = pos
      Seq(
        (r - 1, c - 1),
        (r - 1, c),
        (r - 1, c + 1),
        (r, c - 1),
        (r, c + 1),
        (r + 1, c - 1),
        (r + 1, c),
        (r + 1, c + 1)
      )

    def clamp(size: Int) =
      val (r, c) = pos
      0 <= r && r < size && 0 <= c && c < size

    def isCorner(size: Int) =
      Seq((0, 0), (0, size - 1), (size - 1, 0), (size - 1, size - 1)) contains pos

  case class Grid(lights: Seq[Seq[Light]], size: Int):
    def neighborsOf(row: Int, col: Int) = (row, col).neighbors
      .filter(_.clamp(size))
      .map((r, c) => lights(r)(c))

    lazy val nextLights =
      for row <- 0 until size
      yield for col <- 0 until size
      yield lights(row)(col).next(neighborsOf(row, col))

    lazy val nextLights2 =
      for row <- 0 until size
      yield for
        col <- 0 until size
        pos = (row, col)
      yield
        if pos.isCorner(size) then On
        else lights(row)(col).next(neighborsOf(row, col))

    lazy val next      = Grid(nextLights, size)
    lazy val next2     = Grid(nextLights2, size)
    lazy val howManyOn = lights.foldLeft(0)((total, row) => total + row.count(_ == On))

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Seq[Light] = line.map(_.toLight)
  def parseGrid(lines: Seq[String])       = Grid(lines map parseLine, lines.size)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(steps: Int) =
    var start = Parsing.parseGrid(lines)
    for _ <- 0 until steps do start = start.next
    start.howManyOn

  def solve2(lines: Seq[String])(steps: Int) =
    var start = Parsing.parseGrid(lines)
    for _ <- 0 until steps do start = start.next2
    start.howManyOn

object Test:
  lazy val file1  = os.pwd / "2015" / "18" / "18.test.input.txt"
  lazy val file2  = os.pwd / "2015" / "18" / "18.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)(4)
  lazy val res2   = Solving.solve2(lines2)(5)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 17

object Main:
  lazy val file1  = os.pwd / "2015" / "18" / "18.input.txt"
  lazy val file2  = os.pwd / "2015" / "18" / "18.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)(100)
  lazy val res2   = Solving.solve2(lines2)(100)
// Main.res1 // part 1: 814
// Main.res2 // part 2: 924
