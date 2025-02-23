package aoc2018.day23

object DataDefs:
  type Pos  = (x: Int, y: Int, z: Int)
  type Nano = (pos: Pos, rad: Int)

  extension (p: Pos)
    def dist(that: Pos) = (p.x - that.x).abs + (p.y - that.y).abs + (p.z - that.z).abs

  case class Run(st: Int, end: Int):
    def closest: Int = math.min(st.abs, end.abs)
    val middle: Int  = st + (end - st) / 2
    def split        = Seq(Run(st, middle), Run(middle + 1, end))
    def dist(n: Int) = if n < st then st - n else if n > end then n - end else 0

  case class Cube(xs: Run, ys: Run, zs: Run):
    def closest: Int = xs.closest + ys.closest + zs.closest
    def split: Seq[Cube] =
      for
        nxs <- xs.split
        nys <- ys.split
        nzs <- zs.split
      yield Cube(nxs, nys, nzs)
    def dist(pos: Pos): Int     = xs.dist(pos.x) + ys.dist(pos.y) + zs.dist(pos.z)
    def score(nanos: Seq[Nano]) = this -> nanos.count(nano => dist(nano.pos) <= nano.rad)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Nano = line match
    case s"pos=<$x,$y,$z>, r=$r" => ((x.toInt, y.toInt, z.toInt), r.toInt)

  def parse(lines: Seq[String]): Seq[Nano] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val nanobots  = Parsing.parse(lines)
    val strongest = nanobots.maxBy(_.rad)
    nanobots.count(_.pos.dist(strongest.pos) <= strongest.rad)

  @annotation.tailrec
  def helper(power: Int, cubes: Seq[Cube], nanobots: Seq[Nano]): Int =
    if power < 0 then cubes.map(_.closest).min
    else
      val candidates = cubes.flatMap(_.split).map(_.score(nanobots))
      val max        = candidates.map(_._2).max
      val remaining  = candidates.filter(_._2 == max).map(_._1)
      helper(power - 1, remaining, nanobots)

  def solve2(lines: Seq[String]) =
    val nanos = Parsing.parse(lines)
    val power = 30
    val start = 1 << power
    val run   = Run(-start, start - 1)
    helper(power, Seq(Cube(run, run, run)), nanos)

object Test:
  lazy val file1  = os.pwd / "2018" / "23" / "23.test.input.1.txt"
  lazy val file2  = os.pwd / "2018" / "23" / "23.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)

object Main:
  lazy val file  = os.pwd / "2018" / "23" / "23.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 7
  println(Test.res2) // part 2: 36
  println(Main.res1) // part 1: 319
  println(Main.res2) // part 2: 129293598
