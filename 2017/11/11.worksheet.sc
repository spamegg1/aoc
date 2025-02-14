import scala.math.{abs, max}

object DataDefs:
  enum Dir:
    case N, NE, SE, S, SW, NW
  import Dir.*

  extension (s: String)
    def toDir = s match
      case "n"  => N
      case "ne" => NE
      case "nw" => NW
      case "s"  => S
      case "se" => SE
      case "sw" => SW

  // https://www.redblobgames.com/grids/hexagons/#coordinates-cube
  case class Hex(x: Int, y: Int):
    require((x + y) % 2 == 0)
    lazy val distance = abs(x) + max(0, (abs(y) - abs(x)) / 2)

    def move(dir: Dir): Hex = dir match
      case N  => Hex(x, y + 2)
      case NE => Hex(x + 1, y + 1)
      case SE => Hex(x + 1, y - 1)
      case S  => Hex(x, y - 2)
      case SW => Hex(x - 1, y - 1)
      case NW => Hex(x - 1, y + 1)

object Parsing:
  import DataDefs.*
  def parse(line: String) = line.split(",").map(_.toDir).toSeq

object Solving:
  import DataDefs.*

  def solve1(line: String) = Parsing
    .parse(line)
    .foldLeft(Hex(0, 0))((hex, dir) => hex.move(dir))
    .distance

  private var maxDist = 0

  def solve2(line: String) = Parsing
    .parse(line)
    .foldLeft((Hex(0, 0), 0)): (hexDist, dir) =>
      val (hex, dist) = hexDist
      val next        = hex.move(dir)
      (next, max(dist, next.distance))
    ._2

object Test:
  lazy val file  = os.pwd / "2017" / "11" / "11.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2
// Test.res1 // part 1: 3,0,2,3
// Test.res2 // part 2: 3,2,2,3

object Main:
  lazy val file = os.pwd / "2017" / "11" / "11.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 743
// Main.res2 // part 2: 1493
