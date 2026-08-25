object DataDefs:
  type Row = Seq[Int]
  type Col = Seq[Int]

  case class Grid(trees: Seq[Row]):
    val rows                  = trees.size
    val cols                  = trees.head.size
    def getCol(col: Int): Col = trees.map(_(col))

    def leftVisible(row: Int, col: Int) =
      trees(row).take(col).forall(_ < trees(row)(col))
    def rightVisible(row: Int, col: Int) =
      trees(row).drop(col + 1).forall(_ < trees(row)(col))
    def topVisible(row: Int, col: Int) =
      getCol(col).take(row).forall(_ < trees(row)(col))
    def botVisible(row: Int, col: Int) =
      getCol(col).drop(row + 1).forall(_ < trees(row)(col))

    def isVisible(row: Int, col: Int): Boolean =
      leftVisible(row, col) || rightVisible(row, col) ||
        topVisible(row, col) || botVisible(row, col)

    val countVisible = // part 1
      (for
        row <- 0 until rows
        col <- 0 until cols
        if isVisible(row, col)
      yield 1).sum

    def look(trees: Row, tree: Int): Int =
      val (visible, invisible) = (trees.takeWhile(_ < tree), trees.dropWhile(_ < tree))
      if invisible.nonEmpty then visible.size + 1 else visible.size

    def lookUp(row: Int, col: Int): Int =
      look(getCol(col).take(row).reverse, trees(row)(col))
    def lookDown(row: Int, col: Int): Int =
      look(getCol(col).drop(row + 1), trees(row)(col))
    def lookLeft(row: Int, col: Int): Int =
      look(trees(row).take(col).reverse, trees(row)(col))
    def lookRight(row: Int, col: Int): Int =
      look(trees(row).drop(col + 1), trees(row)(col))

    def scenicScore(row: Int, col: Int): Int =
      lookUp(row, col) * lookDown(row, col) * lookLeft(row, col) * lookRight(row, col)

    val bestScore = // part 2
      (for
        row <- 0 until rows
        col <- 0 until cols
      yield scenicScore(row, col)).max

object Parsing:
  import DataDefs.*
  def parseLine(line: String)   = line.toSeq.map(_.asDigit)
  def parse(lines: Seq[String]) = Grid(lines map parseLine)

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = Parsing.parse(lines).countVisible
  def solve2(lines: Seq[String]) = Parsing.parse(lines).bestScore

object Test:
  val file  = os.pwd / "2022" / "08" / "08.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 21
// Test.res2 // part 2: 8

object Main:
  val file  = os.pwd / "2022" / "08" / "08.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1845
// Main.res2 // part 2: 230112
