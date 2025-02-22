package aoc2018.day22

object DataDefs:
  val mod = 20183L
  val X   = 16807L
  val Y   = 48271L

  enum Typ:
    case Rocky, Wet, Narrow

  type Pos           = (x: Int, y: Int)
  type GeologicIndex = Long
  type ErosionLevel  = Long
  type Region        = (typ: Typ, geo: GeologicIndex, ero: ErosionLevel)
  type Cave          = Map[Pos, Region]

object Cave:
  import DataDefs.*

  def horiz(target: Pos, depth: Long): Cave = (0 to target.x)
    .map: x =>
      val pos = (x, 0)
      val geo = x * X
      val ero = (geo + depth) % mod
      val typ = Typ.values((ero % 3).toInt)
      pos -> (typ, geo, ero)
    .toMap

  def vert(target: Pos, depth: Long): Cave = (0 to target.y)
    .map: y =>
      val pos    = (0, y)
      val geo    = y * Y
      val ero    = (geo + depth) % mod
      val typ    = Typ.values((ero % 3).toInt)
      val region = (typ, geo, ero)
      pos -> region
    .toMap

  def poses(target: Pos): Seq[Pos] =
    for
      x <- 1 to target.x
      y <- 1 to target.y
    yield (x, y)

  def populate(target: Pos, depth: Long): Cave =
    val start: Cave = horiz(target, depth) ++ vert(target, depth)
    val poss        = poses(target)
    poss.foldLeft(start): (cave, pos) =>
      val neighbor1 = (pos.x - 1, pos.y)
      val neighbor2 = (pos.x, pos.y - 1)
      val geo    = if pos == target then 0L else cave(neighbor1).ero * cave(neighbor2).ero
      val ero    = (geo + depth) % mod
      val typ    = Typ.values((ero % 3).toInt)
      val region = (typ, geo, ero)
      cave + (pos -> region)

object Solving:
  import DataDefs.*

  def solve1(target: Pos, depth: Long) = Cave
    .populate(target, depth)
    .values
    .map(_.typ.ordinal) // risk
    .sum

  def solve2(target: Pos, depth: Long) = 0L

object Test:
  lazy val target = (10, 10)
  lazy val depth  = 510L
  lazy val res1   = Solving.solve1(target, depth)
  lazy val res2   = Solving.solve2(target, depth)

object Main:
  lazy val target = (9, 751)
  lazy val depth  = 11817L
  lazy val res1   = Solving.solve1(target, depth)
  lazy val res2   = Solving.solve2(target, depth)

@main
def run: Unit =
  println(Test.res1) // part 1: 114
  println(Test.res2) // part 2: ???
  println(Main.res1) // part 1: 7402
  println(Main.res2) // part 2: ???
