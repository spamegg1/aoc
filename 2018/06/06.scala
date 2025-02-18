package aoc2018.day06

object DataDefs:
  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def dist(that: Pos) = (p.x - that.x).abs + (p.y - that.y).abs
    def neighbors = Seq((p.x, p.y + 1), (p.x, p.y - 1), (p.x - 1, p.y), (p.x + 1, p.y))
    def closestOption(coords: Seq[Pos]): Option[(Pos, Pos)] =
      val sorted  = coords.sortBy(_.dist(p))
      val first   = sorted.head
      val second  = sorted.tail.head
      val closest = first.dist(p) < second.dist(p)
      Option.when(closest)(first -> p)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Pos =
    val Array(x, y) = line.split(", ").map(_.toInt)
    (x = x, y = y)

  def parse(lines: Seq[String]): Seq[Pos] = lines.map(parseLine)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val coords = Parsing.parse(lines)
    val xs     = coords.map(_.x)
    val ys     = coords.map(_.y)
    val left   = xs.min
    val right  = xs.max
    val top    = ys.min
    val bot    = ys.max
    val poses =
      for
        x <- left to right
        y <- top to bot
      yield (x = x, y = y)

    poses
      .flatMap(_.closestOption(coords)) // Seq[(Pos, Pos)]
      .groupMap(_._1)(_._2)             // Map[Pos, Seq[Pos]]
      .filterNot: (_, ps) =>
        ps.exists: p =>
          p.x == left || p.x == right || p.y == top || p.y == bot
      .values      // Iterable[Seq[Pos]]
      .map(_.size) // Iterable[Int]
      .max         // Int

  def solve2(lines: Seq[String])(threshold: Int) =
    val coords  = Parsing.parse(lines)
    val meanX   = coords.map(_.x).sum / coords.size
    val meanY   = coords.map(_.x).sum / coords.size
    val start   = (x = meanX, y = meanY)
    val todo    = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)

    while todo.nonEmpty do
      todo
        .dequeue()
        .neighbors
        .filterNot(visited.contains)
        .filter(next => coords.map(_.dist(next)).sum < threshold)
        .foreach: next =>
          todo.enqueue(next)
          visited += next
    visited.size

object Test:
  lazy val file  = os.pwd / "2018" / "06" / "06.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(32)

object Main:
  lazy val file  = os.pwd / "2018" / "06" / "06.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(10000)

@main
def run: Unit =
  println(Test.res1) // part 1: 17
  println(Test.res2) // part 2: 16
  println(Main.res1) // part 1: 6047
  println(Main.res2) // part 2: 46320
