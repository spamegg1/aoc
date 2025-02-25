package aoc2019.day12

object DataDefs:
  @annotation.tailrec
  def gcd(a: Long, b: Long): Long = if a % b == 0 then b else gcd(b, a % b)
  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  case class Pos(x: Int, y: Int, z: Int):
    def +(that: Pos) = Pos(x + that.x, y + that.y, z + that.z)
    def -(that: Pos) = Pos(x - that.x, y - that.y, z - that.z)
    def sign         = Pos(x.sign, y.sign, z.sign)
    def manhattan    = x.abs + y.abs + z.abs

  case class Moon(pos: Pos, vel: Pos):
    def energy: Int = pos.manhattan * vel.manhattan

  extension (moons: Seq[Moon])
    def next: Seq[Moon] = moons.map: moon =>
      val vel = moons.foldLeft(moon.vel)((pos, m) => pos + (m.pos - moon.pos).sign)
      Moon(moon.pos + vel, vel)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Moon = line match
    case s"<x=$x, y=$y, z=$z>" => Moon(Pos(x.toInt, y.toInt, z.toInt), Pos(0, 0, 0))

  def parse(lines: Seq[String]): Seq[Moon] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(steps: Int) = Iterator
    .iterate(Parsing.parse(lines))(_.next)
    .drop(steps)
    .next()
    .map(_.energy)
    .sum

  @annotation.tailrec
  def halfPeriod(moons: Seq[Moon], dimension: Pos => Int, count: Int = 1): Long =
    val next = moons.next
    if next.map(_.vel).map(dimension).forall(_ == 0) then count
    else halfPeriod(next, dimension, count + 1)

  def solve2(lines: Seq[String]) =
    val moons = Parsing.parse(lines)
    val halfX = halfPeriod(moons, _.x)
    val halfY = halfPeriod(moons, _.y)
    val halfZ = halfPeriod(moons, _.z)
    2 * lcm(lcm(halfX, halfY), halfZ)

object Test:
  lazy val file1  = os.pwd / "2019" / "12" / "12.test.input.1.txt"
  lazy val file2  = os.pwd / "2019" / "12" / "12.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res11  = Solving.solve1(lines1)(10)
  lazy val res12  = Solving.solve1(lines2)(100)
  lazy val res21  = Solving.solve2(lines1)
  lazy val res22  = Solving.solve2(lines2)

object Main:
  lazy val file  = os.pwd / "2019" / "12" / "12.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(1000)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res11) // part 1: 179
  println(Test.res12) // part 1: 1940
  println(Test.res21) // part 2: 2772
  println(Test.res22) // part 2: 4686774924
  println(Main.res1)  // part 1: 7687
  println(Main.res2)  // part 2: 334945516288044
