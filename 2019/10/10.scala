package aoc2019.day10

object DataDefs:
  type Pos  = (x: Int, y: Int)
  type Line = (a: Int, b: Int, c: Int)
  type Data = (Int, Int, Int)
  type Dist = Map[Pos, Int]

  extension (p: Pos)
    def to(that: Pos): Line = (p.y - that.y, that.x - p.x, p.x * that.y - that.x * p.y)

  extension (l: Line)
    def distance(p: Pos): Int = l.a * p.x + l.b * p.y + l.c
    def normal(p: Pos): Line  = (l.b, -l.a, l.a * p.y - l.b * p.x)

  extension (ps: Set[Pos])
    def groupByLine(origin: Pos): Map[Line, Dist] =
      (ps - origin)
        .foldLeft(Map.empty[Line, List[Pos]]): (lines, pos) =>
          lines.keys.find(_.distance(pos) == 0) match
            case Some(line) => lines.updated(line, pos :: lines(line))
            case None       => lines.updated(origin.to(pos), List(pos))
        .map: (line, poses) =>
          val normal = line.normal(origin)
          line -> poses.zip(poses.map(normal.distance)).toMap

    def countVisible: Set[(Pos, Int)] =
      ps.map: origin =>
        val visible = ps
          .groupByLine(origin) // Map[Line, Dist]
          .values              // Iterable[Dist]
          .map: pairs =>       // Map[Pos, Int]
            val distances = pairs.values // Iterable[Int]
            distances.count(_ < 0).min(1) + distances.count(_ > 0).min(1)
        origin -> visible.sum

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Set[Pos] =
    (for
      x <- 0 until lines.head.size
      y <- 0 until lines.size
      if lines(y)(x) == '#'
    yield (x, y)).toSet

object Solving:
  import DataDefs.*

  def clockwise(quad: Int, origin: Pos, ps: Set[Pos]): Map[Data, Pos] = ps
    .groupByLine(origin)
    .flatMap: (line, distances) =>
      val toLeft  = ps.count(pos => line.distance(pos) < 0)
      val inOrder = distances.values.toSeq.sorted.zipWithIndex.toMap
      distances.map((pos, distance) => (inOrder(distance), quad, toLeft) -> pos)

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .countVisible
    .maxBy(_._2)
    ._2

  def solve2(lines: Seq[String])(count: Int) =
    val ps                = Parsing.parse(lines)
    val (origin, visible) = ps.countVisible.maxBy(_._2)
    val topRight = clockwise(0, origin, ps.filter(p => p.x >= origin.x && p.y < origin.y))
    val botRight = clockwise(1, origin, ps.filter(p => p.x > origin.x && p.y >= origin.y))
    val botLeft  = clockwise(2, origin, ps.filter(p => p.x <= origin.x && p.y > origin.y))
    val topLeft  = clockwise(3, origin, ps.filter(p => p.x < origin.x && p.y <= origin.y))
    val all      = topRight ++ botRight ++ botLeft ++ topLeft
    val key      = all.keys.toSeq.sorted.apply(count - 1)
    val result   = all(key)
    100 * result.x + result.y

object Test:
  lazy val file  = os.pwd / "2019" / "10" / "10.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(200)

object Main:
  lazy val file  = os.pwd / "2019" / "10" / "10.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(200)

@main
def run: Unit =
  println(Test.res1) // part 1: 210
  println(Test.res2) // part 2: 802
  println(Main.res1) // part 1: 227
  println(Main.res2) // part 2: 604
