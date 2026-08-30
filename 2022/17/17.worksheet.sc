object DataDefs:
  case class Point(x: Int, y: Int)

  val Shapes = Seq(
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)),
    Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)),
    Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)),
    Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))
  )

  extension (shape: Set[Point])
    def move(dx: Int, dy: Int): Set[Point] = shape.map(p => Point(p.x + dx, p.y + dy))
    def canMove(grid: Set[Point]): Boolean =
      shape.forall(p => p.x > 0 && p.x < 8 && !grid.contains(p))

  case class State(
      jets: String,
      grid: Set[Point],
      shapeIndex: Int,
      jetIndex: Int,
      height: Int
  ):
    def step: State =
      val initialShape              = Shapes(shapeIndex % Shapes.size).move(3, height + 4)
      val (nextShape, nextJetIndex) = fall(initialShape, jetIndex)
      val nextHeight                = height.max(nextShape.map(_.y).max)
      State(jets, grid ++ nextShape, shapeIndex + 1, nextJetIndex, nextHeight)

    @annotation.tailrec
    private def fall(shape: Set[Point], jetIndex: Int): (Set[Point], Int) =
      val jet    = jets(jetIndex % jets.length)
      val first  = if jet == '>' then shape.move(1, 0) else shape.move(-1, 0)
      val second = if first.canMove(grid) then first else shape
      val third  = second.move(0, -1)
      if third.canMove(grid) then fall(third, jetIndex + 1) else (second, jetIndex + 1)
  end State
end DataDefs

object Parsing:
  import DataDefs.*

  def parse(line: String) =
    val initial = State(line, Set.tabulate(8)(Point(_, 0)), 0, 0, 0)
    Iterator
      .iterate(initial)(_.step)
      .map(_.height)

object Solving:
  import DataDefs.*

  def solve1(rocks: Int)(line: String) = Parsing
    .parse(line)
    .drop(rocks)
    .next()

  def solve2(rocks: Long)(line: String) =
    val guess       = 1000
    val height      = Parsing.parse(line).slice(1, 5 * guess).toSeq
    val delta       = height.sliding(2).map(s => s.last - s.head).toSeq
    val end         = delta.size - guess
    val start       = delta.lastIndexOfSlice(delta.takeRight(guess), end - 1)
    val cycleHeight = height(end) - height(start)
    val cycleWidth  = end - start
    val offset      = rocks - 1 - start
    val quotient    = offset / cycleWidth
    val remainder   = offset % cycleWidth
    (quotient * cycleHeight) + height(start + remainder.toInt)
end Solving

object Test:
  val file = os.pwd / "2022" / "17" / "17.test.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(2022)(line)
  val res2 = Solving.solve2(1000000000000L)(line)
// Test.res1 // part 1: 3068
// Test.res2 // part 2: 1514285714288

object Main:
  val file = os.pwd / "2022" / "17" / "17.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(2022)(line)
  val res2 = Solving.solve2(1000000000000L)(line)
// Main.res1 // part 1: 3141
// Main.res2 // part 2: 1561739130391
