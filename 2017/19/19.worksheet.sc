/*
--- Day 19: A Series of Tubes ---
Somehow, a network packet got lost and ended up here.
It's trying to follow a routing diagram (your puzzle input),
but it's confused about where to go.

Its starting point is just off the top of the diagram.
Lines (drawn with |, -, and +) show the path it needs to take,
starting by going down onto the only line connected to the top of the diagram.
It needs to follow this path until it reaches the end
(located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases,
it needs to continue going the same direction,
and only turn left or right when there's no other option.
In addition, someone has left letters on the line;
these also don't change its direction,
but it can use them to keep track of where it's been.
For example:

     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+

Given this diagram, the packet needs to take the following path:
  Starting at the only line touching the top of the diagram,
    it must go down, pass through A, and continue onward to the first +.
  Travel right, up, and right, passing through B in the process.
  Continue down (collecting C), right, and up (collecting D).
  Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way.
What letters will it see (in the order it would see them) if it follows the path?
(The routing diagram is very wide; make sure you view it without line wrapping.)

 */
object DataDefs:
  enum Dir:
    case N, S, E, W
  import Dir.*

  case class Packet(dir: Dir, cur: Char, row: Int, col: Int, seen: List[Char]):
    def allSeen = seen.reverse.mkString
    def done(target: String) = target.toSet == seen.toSet
    def next = dir match
      case N =>
      case S =>
      case E =>
      case W =>

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(start: (Int, Int))(target: String) = 0L
  def solve2(lines: Seq[String])(start: (Int, Int))(target: String) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2017" / "19" / "19.test.input.txt")
  private lazy val start = (0, 5)
  private lazy val target = "ABCDEF"
  lazy val result1 = Solving.solve1(lines)(start)(target)
  lazy val result2 = Solving.solve2(lines)(start)(target)
// Testing.result1 // part 1: ABCDEF
// Testing.result2 // part 2:

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2017" / "19" / "19.input.txt")
  private lazy val start = (0, 115)
  private lazy val target = "ABFHMPTYZ"
  lazy val result1 = Solving.solve1(lines)(start)(target)
  lazy val result2 = Solving.solve2(lines)(start)(target)
// Main.result1 // part 1: PBAZMFY
// Main.result2 // part 2:
