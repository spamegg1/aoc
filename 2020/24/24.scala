package aoc2020.day24

object DataDefs:
  case class Tile(x: Int, y: Int):
    val even = y % 2 == 0
    def east = Tile(x + 1, y)
    def west = Tile(x - 1, y)
    def ne   = if even then Tile(x + 1, y + 1) else Tile(x, y + 1)
    def nw   = if even then Tile(x, y + 1) else Tile(x - 1, y + 1)
    def se   = if even then Tile(x + 1, y - 1) else Tile(x, y - 1)
    def sw   = if even then Tile(x, y - 1) else Tile(x - 1, y - 1)
    def nghs = Seq(this, east, west, ne, nw, se, sw)

  type Floor = Set[Tile]
  extension (floor: Floor)
    def adjacent(tile: Tile): Int = tile.nghs.count(floor.contains)
    def step: Floor = floor
      .flatMap(_.nghs)
      .flatMap: tile =>
        (floor.contains(tile), floor.adjacent(tile)) match
          case (_, 2) | (true, 3) => Some(tile)
          case _                  => None

object Parsing:
  import DataDefs.*

  @annotation.tailrec
  def helper(point: Tile, remain: String): Tile =
    if remain.isEmpty then point
    else
      remain.head match
        case 'e' => helper(point.east, remain.tail)
        case 'w' => helper(point.west, remain.tail)
        case 'n' =>
          remain(1) match
            case 'e' => helper(point.ne, remain.drop(2))
            case 'w' => helper(point.nw, remain.drop(2))
        case 's' =>
          remain(1) match
            case 'e' => helper(point.se, remain.drop(2))
            case 'w' => helper(point.sw, remain.drop(2))

  def parse(lines: Seq[String]): Floor = lines
    .map(directions => helper(Tile(0, 0), directions))
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .filter(_._2 % 2 == 1)
    .keySet

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing.parse(lines).size
  def solve2(lines: Seq[String]) = Iterator
    .iterate(Parsing.parse(lines))(step)
    .drop(100)
    .next()
    .size

object Test:
  lazy val file  = os.pwd / "2020" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 10
  println(Test.res2) // part 2: 2208
  println(Main.res1) // part 1: 388
  println(Main.res2) // part 2: 4002
