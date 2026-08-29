object DataDefs:
  val Ortho = Seq((1, 0), (-1, 0), (0, 1), (0, -1))

  type Pos  = (x: Int, y: Int)
  type Grid = Map[Pos, Char]

  extension (p: Pos) def delta(dx: Int, dy: Int): Pos = (p.x + dx, p.y + dy)
  extension (grid: Grid)
    def neighbours(p: Pos): Seq[Pos] = Ortho.map(p.delta).filter(grid.contains)
    def height(p: Pos): Char = grid(p) match
      case 'S' => 'a'
      case 'E' => 'z'
      case ch  => ch
end DataDefs

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Grid =
    (for
      y <- lines.indices
      x <- lines.head.indices
    yield (x, y) -> lines(y)(x)).toMap

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def bfs(grid: Grid, end: Char): Int = boundary:
    val start = grid.map(_.swap)('E')
    val todo  = collection.mutable.Queue(start)
    val cost  = collection.mutable.Map(start -> 0)

    while todo.nonEmpty do
      val point = todo.dequeue()
      if grid(point) == end then break(cost(point))
      else
        grid
          .neighbours(point)
          .filterNot(cost.contains)
          .foreach: next =>
            if grid.height(point) - grid.height(next) <= 1 then
              todo.enqueue(next)
              cost(next) = cost(point) + 1
    end while
    -1
  end bfs

  def solve(char: Char)(lines: Seq[String]) = bfs(Parsing.parse(lines), char)

  val solve1 = solve('S')
  val solve2 = solve('a')
end Solving

object Test:
  val file  = os.pwd / "2022" / "12" / "12.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 31
// Test.res2 // part 2: 29

object Main:
  val file  = os.pwd / "2022" / "12" / "12.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 352
// Main.res2 // part 2: 345
