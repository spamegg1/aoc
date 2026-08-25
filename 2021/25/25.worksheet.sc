object DataDefs:
  type Pos   = (x: Int, y: Int)
  type Cukes = Map[Pos, Char]
  case class Trench(w: Int, h: Int, cukes: Cukes):
    def step: Trench =
      val east = cukes.map:
        case (point, kind) =>
          val next = ((point.x + 1) % w, point.y)
          if kind == '>' && !cukes.contains(next)
          then next  -> kind
          else point -> kind
      val south = east.map:
        case (point, kind) =>
          val next = (point.x, (point.y + 1) % h)
          if kind == 'v' && !east.contains(next)
          then next  -> kind
          else point -> kind
      copy(cukes = south)
    end step
  end Trench
end DataDefs

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Trench =
    val width  = lines.head.size
    val height = lines.size
    val points =
      for
        x <- 0 until width
        y <- 0 until height
        if lines(y)(x) != '.'
      yield (x, y) -> lines(y)(x)
    Trench(width, height, points.toMap)
end Parsing

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def loop(trench: Trench, steps: Int): Int =
    val next = trench.step
    if next == trench then steps else loop(next, steps + 1)

  def solve1(lines: Seq[String]) = loop(Parsing.parse(lines), 1)

object Test:
  val file  = os.pwd / "2021" / "25" / "25.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
// Test.res1 // part 1: 58

object Main:
  val file  = os.pwd / "2021" / "25" / "25.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
// Main.res1 // part 1: 534
