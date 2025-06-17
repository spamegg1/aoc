package aoc2020.day17

object DataDefs:
  type Pos       = (x: Int, y: Int, z: Int, w: Int)
  type Dirs      = Set[Pos]
  type Grid      = Set[Pos]
  type Neighbors = Set[Pos]

  extension (p: Pos)
    def updated(del: Pos): Pos = (p.x + del.x, p.y + del.y, p.z + del.z, p.w + del.w)
    def neighbors(dirs: Dirs): Neighbors = dirs.map(updated)

  extension (g: Grid)
    def rule(dirs: Dirs)(p: Pos) =
      val count = p.neighbors(dirs).count(g.contains)
      count == 3 || (g.contains(p) && count == 4)
    def step(dirs: Dirs): Grid = g
      .flatMap(_.neighbors(dirs))
      .filter(g.rule(dirs))

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Grid =
    val width  = lines.head.size
    val height = lines.size
    val points =
      for
        x <- 0 until width
        y <- 0 until height
        if lines(y)(x) == '#'
      yield (x, y, 0, 0)
    points.toSet

object Solving:
  import DataDefs.*

  def solve(dirs: Dirs)(lines: Seq[String]) = Iterator
    .iterate(Parsing.parse(lines))(_.step(dirs))
    .drop(6)
    .next
    .size

  val dirs1: Dirs = Seq
    .tabulate(3, 3, 3): (x, y, z) =>
      (x - 1, y - 1, z - 1, 0)
    .flatten
    .flatten
    .toSet

  val dirs2: Dirs = Seq
    .tabulate(3, 3, 3, 3): (x, y, z, w) =>
      (x - 1, y - 1, z - 1, w - 1)
    .flatten
    .flatten
    .flatten
    .toSet

  val solve1 = solve(dirs1)
  val solve2 = solve(dirs2)

object Test:
  lazy val file  = os.pwd / "2020" / "17" / "17.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "17" / "17.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 112
  println(Test.res2) // part 2: 848
  println(Main.res1) // part 1: 223
  println(Main.res2) // part 2: 1884
