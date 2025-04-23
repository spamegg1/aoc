package aoc2019.day24

object DataDefs:
  type Pos       = (x: Int, y: Int, z: Int)
  type Grid      = Set[Pos]
  type Neighbors = Pos => Seq[Pos]

  extension (p: Pos)
    def delta(dx: Int, dy: Int): Pos = (p.x + dx, p.y + dy, p.z)
    def orthogonal: Seq[Pos]         = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(p.delta)
    def neighbours2D: Seq[Pos] =
      p.orthogonal.filter(q => q.x >= 0 && q.x < 5 && q.y >= 0 && q.y < 5)
    def neighbours3D: Seq[Pos] = p.orthogonal.flatMap:
      case (-1, y, z) => Set((1, 2, z - 1))
      case (5, y, z)  => Set((3, 2, z - 1))
      case (x, -1, z) => Set((2, 1, z - 1))
      case (x, 5, z)  => Set((2, 3, z - 1))
      case (2, 2, z) =>
        p match
          case (2, 1, z) => Set.tabulate(5)(x => (x, 0, z + 1))
          case (2, 3, z) => Set.tabulate(5)(x => (x, 4, z + 1))
          case (1, 2, z) => Set.tabulate(5)(y => (0, y, z + 1))
          case (3, 2, z) => Set.tabulate(5)(y => (4, y, z + 1))
      case other => Set(other)

  extension (g: Grid)
    def step(neighbors: Neighbors): Grid =
      val candidates = g ++ g.flatMap(neighbors)
      candidates.flatMap: pos =>
        (g.contains(pos), neighbors(pos).count(g.contains)) match
          case (_, 1)     => Some(pos)
          case (false, 2) => Some(pos)
          case _          => None

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Set[Pos] = Set
    .tabulate(5, 5)((x, y) => Option.when(lines(y)(x) == '#')((x, y, 0)))
    .flatten
    .flatten

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def helper(grid: Grid, prev: Set[Grid]): Int =
    val next = grid.step(_.neighbours2D)
    if prev.contains(next)
    then next.map(pos => 1 << (pos.x + 5 * pos.y)).sum
    else helper(next, prev + next)

  def solve1(lines: Seq[String]) = helper(Parsing.parse(lines), Set())

  def solve2(lines: Seq[String])(minutes: Int) = Iterator
    .iterate(Parsing.parse(lines))(_.step(_.neighbours3D))
    .drop(minutes)
    .next()
    .size

object Test:
  lazy val file  = os.pwd / "2019" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(200)

object Main:
  lazy val file  = os.pwd / "2019" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(200)

@main
def run: Unit =
  println(Test.res1) // part 1: 2129920
  println(Test.res2) // part 2: 1922
  println(Main.res1) // part 1: 32506764
  println(Main.res2) // part 2: 1963
