/*
--- Day 11: Hex Ed ---
Crossing the bridge, you've barely reached the other side of the
stream when a program comes up to you, clearly in distress.
"It's my child process," she says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.
Unfortunately for you, it's a hex grid.
The hexagons ("hexes") in this grid are aligned such that adjacent hexes
can be found to the north, northeast, southeast, south, southwest, and northwest:
  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \
You have the path the child process took. Starting where he started,
you need to determine the fewest number of steps required to reach him.
(A "step" means to move from the hex you are in to any adjacent hex.)
For example:
  ne,ne,ne is 3 steps away.
  ne,ne,sw,sw is 0 steps away (back where you started).
  ne,ne,s,s is 2 steps away (se,se).
  se,sw,se,sw,sw is 3 steps away (s,s,sw).

--- Part Two ---
How many steps away is the furthest he ever got from his starting position?
 */
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
      val next = hex.move(dir)
      (next, max(dist, next.distance))
    ._2

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "11.test.input.txt")
  lazy val result1 = lines map Solving.solve1
  lazy val result2 = lines map Solving.solve2
Testing.result1 // part 1: 3,0,2,3
Testing.result2 // part 2: 3,2,2,3

object Main:
  private lazy val line = os.read.lines(os.pwd / "11.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
Main.result1 // part 1: 743
Main.result2 // part 2: 1493
