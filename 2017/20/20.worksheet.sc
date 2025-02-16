object DataDefs:
  type Pos = (x: Int, y: Int, z: Int)
  extension (p: Pos)
    def +(that: Pos) = (p.x + that.x, p.y + that.y, p.z + that.z)
    def manhattan    = p.x.abs + p.y.abs + p.z.abs

  type Particle = (pos: Pos, vel: Pos, acc: Pos)
  extension (p: Particle)
    def step = (pos = p.pos + p.vel + p.acc, vel = p.vel + p.acc, acc = p.acc)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Particle = line match
    case s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>" =>
      val pos = (px.toInt, py.toInt, pz.toInt)
      val vel = (vx.toInt, vy.toInt, vz.toInt)
      val acc = (ax.toInt, ay.toInt, az.toInt)
      (pos, vel, acc)

  def parse(lines: Seq[String]): Seq[Particle] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val particles = Parsing.parse(lines)
    Iterator
      .iterate(particles)(_.map(_.step))
      .drop(500)
      .next()
      .zipWithIndex
      .minBy(_._1.pos.manhattan)
      ._2

  def solve2(lines: Seq[String]) =
    val particles = Parsing.parse(lines)
    Iterator
      .iterate(particles): ps =>
        ps
          .map(_.step)
          .groupBy(_.pos)
          .values
          .filter(_.size == 1)
          .flatten
          .toSeq
      .drop(500)
      .next()
      .size

object Test:
  lazy val file  = os.pwd / "2017" / "20" / "20.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 0
// Test.res2 // part 2: 4

object Main:
  lazy val file  = os.pwd / "2017" / "20" / "20.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 376
// Main.res2 // part 2: 574
