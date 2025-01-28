/*
--- Day 3: Crossed Wires ---
The gravity assist was successful, and you're well on your way to the Venus refuelling
station. During the rush back on Earth, the fuel management system wasn't completely
installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected
to a central port and extend outward on a grid. You trace the path each wire takes as it
leaves the central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit,
you need to find the intersection point closest to the central port. Because the wires are
on a grid, use the Manhattan distance for this measurement. While the wires do technically
cross right at the central port where they both start, this point does not count, nor does
a wire count as crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port
(o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

These wires cross at two locations (marked X), but the lower-left one is closer to the
central port: its distance is 3 + 3 = 6.

Here are a few more examples:
  R75,D30,R83,U83,L12,D49,R71,U7,L72
  U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
  R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

What is the Manhattan distance from the central port to the closest intersection?

--- Part Two ---
It turns out that this circuit is very timing-sensitive;
you actually need to minimize the signal delay.

To do this, calculate the number of steps each wire takes to reach each intersection;
choose the intersection where the sum of both wires' steps is lowest.
If a wire visits a position on the grid multiple times, use the steps value from the first
time it visits that position when calculating the total value of a specific intersection.

The number of steps a wire takes is the total number of grid squares the wire has
entered to get to that location, including the intersection being considered.
Again consider the example from above:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

In the above example, the intersection closest to the central port
is reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps
by the second wire for a total of 20+20 = 40 steps.

However, the top-right intersection is better: the first wire takes only 8+5+2 = 15
and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.

Here are the best steps for the extra examples from above:
  R75,D30,R83,U83,L12,D49,R71,U7,L72
  U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
  R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
  U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

What is the fewest combined steps the wires must take to reach an intersection?
 */
object DataDefs:
  enum Dir:
    case Left, Right, Up, Down
  import Dir.*

  extension (c: Char)
    def toDir = c match
      case 'L' => Left
      case 'R' => Right
      case 'U' => Up
      case 'D' => Down

  case class Pos(x: Int, y: Int):
    lazy val distance = math.abs(x) + math.abs(y)
    def steps(wires: Seq[Wire]) = wires.head.steps(this) + wires.last.steps(this)

  enum Segment:
    case Vertical(x: Int, yRange: Range, start: Pos)
    case Horizontal(xRange: Range, y: Int, start: Pos)

    def intersect(that: Segment): Seq[Pos] = (this, that) match
      case (Vertical(x1, y1Range, _), Vertical(x2, y2Range, _)) =>
        if x1 == x2 then y1Range.intersect(y2Range).map(y => Pos(x1, y)) else Seq()
      case (Vertical(x1, y1Range, _), Horizontal(x2Range, y2, _)) =>
        if x2Range.contains(x1) && y1Range.contains(y2) then Seq(Pos(x1, y2)) else Seq()
      case (Horizontal(x1Range, y1, _), Vertical(x2, y2Range, _)) =>
        if x1Range.contains(x2) && y2Range.contains(y1) then Seq(Pos(x2, y1)) else Seq()
      case (Horizontal(x1Range, y1, _), Horizontal(x2Range, y2, _)) =>
        if y1 == y2 then x1Range.intersect(x2Range).map(x => Pos(x, y1)) else Seq()

    def contains(pos: Pos): Boolean = this match
      case Vertical(x, yRange, _)   => x == pos.x && yRange.contains(pos.y)
      case Horizontal(xRange, y, _) => y == pos.y && xRange.contains(pos.x)

    def distance(pos: Pos): Int = this match // assume this contains pos
      case Vertical(x, _, start)   => math.abs(pos.y - start.y)
      case Horizontal(_, y, start) => math.abs(pos.x - start.x)

    lazy val length = this match
      case Vertical(_, yRange, _)   => yRange.length
      case Horizontal(xRange, _, _) => xRange.length
  import Segment.*

  case class Move(dir: Dir, distance: Int):
    def toSegment(p: Pos): (Segment, Pos) = dir match
      case Left =>
        val end = Pos(p.x - distance, p.y)
        (Horizontal(Range.inclusive(end.x, p.x), p.y, p), end)
      case Right =>
        val end = Pos(p.x + distance, p.y)
        (Horizontal(Range.inclusive(p.x, end.x), p.y, p), end)
      case Up =>
        val end = Pos(p.x, p.y + distance)
        (Vertical(p.x, Range.inclusive(p.y, end.y), p), end)
      case Down =>
        val end = Pos(p.x, p.y - distance)
        (Vertical(p.x, Range.inclusive(end.y, p.y), p), end)

  extension (s: String) def toMove = Move(s.head.toDir, s.tail.toInt)

  case class Wire(moves: Seq[Move]):
    import util.boundary, boundary.break

    private lazy val segments: Seq[Segment] =
      var pos = Pos(0, 0)
      var res = Seq[Segment]()
      for move <- moves do
        val (segment, newPos) = move.toSegment(pos)
        pos = newPos
        res = segment +: res
      res

    def intersect(that: Wire): Seq[Pos] =
      segments.flatMap(segment => that.segments.flatMap(_.intersect(segment)))

    def steps(target: Pos): Int =
      var pos = Pos(0, 0)
      var res = 0
      boundary:
        for move <- moves do
          val (segment, newPos) = move.toSegment(pos)
          pos = newPos
          if segment.contains(target) then
            res += segment.distance(target)
            break()
          else res += segment.length - 1
      res

object Parsing:
  import DataDefs.*
  private def parseWire(line: String): Seq[Move] = line.split(",").map(_.toMove).toSeq
  def parseWires(lines: Seq[String]): Seq[Wire] = lines.map(line => Wire(parseWire(line)))

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val wires = Parsing.parseWires(lines)
    wires.head
      .intersect(wires.last)
      .diff(Seq(Pos(0, 0)))
      .map(_.distance)
      .min

  def solve2(lines: Seq[String]) =
    val wires = Parsing.parseWires(lines)
    wires.head
      .intersect(wires.last)
      .diff(Seq(Pos(0, 0)))
      .map(_.steps(wires))
      .min

object Test:
  private lazy val lines1 = os.read.lines(os.pwd / "2019" / "03" / "03.test.input.1.txt")
  private lazy val lines2 = os.read.lines(os.pwd / "2019" / "03" / "03.test.input.2.txt")
  private lazy val lines3 = os.read.lines(os.pwd / "2019" / "03" / "03.test.input.3.txt")
  lazy val res11 = Solving.solve1(lines1)
  lazy val res12 = Solving.solve1(lines2)
  lazy val res13 = Solving.solve1(lines3)
  lazy val res21 = Solving.solve2(lines1)
  lazy val res22 = Solving.solve2(lines2)
  lazy val res23 = Solving.solve2(lines3)
// Test.res11 // part 1: 6 = (3, 3)
// Test.res12 // part 1: 159 = (155, 4)
// Test.res13 // part 1: 135
// Test.res21 // part 2: 30
// Test.res22 // part 2: 610
// Test.res23 // part 2: 410

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2019" / "03" / "03.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 258
// Main.res2 // part 2: 12304
