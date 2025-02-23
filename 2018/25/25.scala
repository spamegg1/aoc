package aoc2018.day25

object DataDefs:
  type Pos = (x: Int, y: Int, z: Int, w: Int)
  extension (p: Pos)
    def dist(that: Pos): Int =
      (p.x - that.x).abs + (p.y - that.y).abs + (p.z - that.z).abs + (p.w - that.w).abs

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Pos = line match
    case s"$x,$y,$z,$w" => (x.toInt, y.toInt, z.toInt, w.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) = Parsing
    .parse(lines)
    .foldLeft(Set.empty[Set[Pos]]): (constellations, pos) =>
      val (near, far) = constellations.partition(_.exists(_.dist(pos) <= 3))
      far + (near.flatten + pos)
    .size

object Test:
  lazy val file1 = os.pwd / "2018" / "25" / "25.test.input.1.txt"
  lazy val file2 = os.pwd / "2018" / "25" / "25.test.input.2.txt"
  lazy val file3 = os.pwd / "2018" / "25" / "25.test.input.3.txt"
  lazy val files = Seq(file1, file2, file3)
  lazy val lines = files map os.read.lines
  lazy val res   = lines map Solving.solve

object Main:
  lazy val file  = os.pwd / "2018" / "25" / "25.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

@main
def run: Unit =
  println(Test.res) // part 1: 4,3,8
  println(Main.res) // part 1: 381
