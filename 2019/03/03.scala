package aoc2019.day03

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
    lazy val distance           = math.abs(x) + math.abs(y)
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
  lazy val file1 = os.pwd / "2019" / "03" / "03.test.input.1.txt"
  lazy val file2 = os.pwd / "2019" / "03" / "03.test.input.2.txt"
  lazy val file3 = os.pwd / "2019" / "03" / "03.test.input.3.txt"
  lazy val files = Seq(file1, file2, file3)
  lazy val lines = files map os.read.lines
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2

object Main:
  lazy val file  = os.pwd / "2019" / "03" / "03.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 6 = (3, 3); 159 = (155, 4); 135
  println(Test.res2) // part 2: 30, 610, 410
  println(Main.res1) // part 1: 258
  println(Main.res2) // part 2: 12304
