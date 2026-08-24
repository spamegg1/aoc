package aoc2021.day22

object DataDefs:
  type Points = Set[(Int, Int, Int)]

  case class Reboot(on: Boolean, cuboid: Cuboid):
    def next(pts: Points, bounds: Cuboid): Points = cuboid
      .intersect(bounds)
      .map: cube =>
        val nextPts = cube.next
        if on then pts ++ nextPts else pts -- nextPts
      .getOrElse(pts)
  end Reboot

  case class Cuboid(xs: Dim, ys: Dim, zs: Dim):
    def volume: Long = xs.size * ys.size * zs.size

    def intersect(other: Cuboid): Option[Cuboid] =
      for
        nextXs <- xs.intersect(other.xs)
        nextYs <- ys.intersect(other.ys)
        nextZs <- zs.intersect(other.zs)
      yield Cuboid(nextXs, nextYs, nextZs)

    def split(other: Cuboid): List[Cuboid] =
      val headX :: tailX = xs.split(other.xs).runtimeChecked
      val headY :: tailY = ys.split(other.ys).runtimeChecked
      val headZ :: tailZ = zs.split(other.zs).runtimeChecked
      tailX.map(Cuboid(_, ys, zs)) ++
        tailY.map(Cuboid(headX, _, zs)) ++
        tailZ.map(Cuboid(headX, headY, _))

    def next =
      for
        x <- xs.range
        y <- ys.range
        z <- zs.range
      yield (x, y, z)
  end Cuboid

  case class Dim(from: Int, to: Int):
    val (start, end) = if from <= to then (from, to) else (to, from)
    def size: Long   = 1 + end - start
    def range: Range = start to end

    def intersect(other: Dim): Option[Dim] =
      val (s1, e1) = (start, end)
      val (s2, e2) = (other.start, other.end)
      if s2 <= s1 && e1 <= e2 then Some(this)
      else if s1 < s2 && e2 < e1 then Some(other)
      else if s2 <= s1 && s1 <= e2 then Some(Dim(s1, e2))
      else if s1 <= s2 && s2 <= e1 then Some(Dim(s2, e1))
      else None

    def split(other: Dim): List[Dim] =
      val (s1, e1) = (start, end)
      val (s2, e2) = (other.start, other.end)
      if s2 <= s1 && e1 <= e2 then List(this)
      else if s1 < s2 && e2 < e1 then List(Dim(s2, e2), Dim(s1, s2 - 1), Dim(e2 + 1, e1))
      else if s1 <= e2 && e2 < e1 then List(Dim(s1, e2), Dim(e2 + 1, e1))
      else if s1 < s2 && s2 <= e1 then List(Dim(s2, e2), Dim(s1, s2 - 1))
      else List()
  end Dim
end DataDefs

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line match
    case s"$state x=$fromX..$toX,y=$fromY..$toY,z=$fromZ..$toZ" =>
      val xs = Dim(fromX.toInt, toX.toInt)
      val ys = Dim(fromY.toInt, toY.toInt)
      val zs = Dim(fromZ.toInt, toZ.toInt)
      Reboot(state == "on", Cuboid(xs, ys, zs))

  def parse(lines: Seq[String]) = lines.map(parseLine).toList

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(region: Int) =
    val range  = Dim(-region, region)
    val bounds = Cuboid(range, range, range)
    val start  = Set.empty[(Int, Int, Int)]
    Parsing
      .parse(lines)
      .foldLeft(start)((pts, rbt) => rbt.next(pts, bounds))
      .size

  @annotation.tailrec
  def algo(todo: List[Reboot], reactor: List[Cuboid]): List[Cuboid] = todo match
    case Nil => reactor
    case Reboot(true, head) :: tail =>
      reactor.view
        .map(_.intersect(head))
        .flatten
        .headOption match
        case None => algo(tail, head :: reactor)
        case Some(intersect) =>
          val remaining = head.split(intersect).map(Reboot(true, _))
          algo(remaining ++ tail, reactor)
    case Reboot(false, head) :: tail =>
      val nextReactor = reactor.flatMap: cuboid =>
        cuboid.intersect(head) match
          case None            => List(cuboid)
          case Some(intersect) => cuboid.split(intersect)
      algo(tail, nextReactor)
  end algo

  def solve2(lines: Seq[String]) =
    algo(Parsing.parse(lines), Nil)
      .map(_.volume)
      .sum

object Test:
  val file  = os.pwd / "2021" / "22" / "22.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(50)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2021" / "22" / "22.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(50)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 590784
  println(Test.res2) // part 2: 39769202357779
  println(Main.res1) // part 1: 611176
  println(Main.res2) // part 2: 1201259791805392
