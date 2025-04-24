object DataDefs:
  extension (c: Char) def isTree = c == '#'
  type Slope = (Int, Int)

  case class Pos(x: Int, y: Int):
    def next(slope: Slope, height: Int, width: Int): Pos =
      val (right, down) = slope
      Pos((x + right) % width, y + down)
    def isInBounds(height: Int): Boolean = y < height

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(slope: Slope): Long =
    val height: Int = lines.size
    val width: Int  = lines(0).size
    var pos         = Pos(0, 0)
    var count       = 0
    while pos.isInBounds(height) do
      if lines(pos.y)(pos.x).isTree then count += 1
      pos = pos.next(slope, height, width)
    count.toLong

  def solve2(lines: Seq[String])(slopes: List[Slope]): Long =
    slopes.map(slope => solve1(lines)(slope)).product

object Test:
  lazy val file   = os.pwd / "2020" / "03" / "03.test.input.txt"
  lazy val lines  = os.read.lines(file)
  lazy val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  lazy val res1   = Solving.solve1(lines)(slopes(1))
  lazy val res2   = Solving.solve2(lines)(slopes)
// Test.res1 // part 1: 7
// Test.res2 // part 2: 336

object Main:
  lazy val file   = os.pwd / "2020" / "03" / "03.input.txt"
  lazy val lines  = os.read.lines(file)
  lazy val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  lazy val res1   = Solving.solve1(lines)(slopes(1))
  lazy val res2   = Solving.solve2(lines)(slopes)
// Main.res1 // part 1: 244
// Main.res2 // part 2: 9406609920
