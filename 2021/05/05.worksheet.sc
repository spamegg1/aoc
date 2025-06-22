object DataDefs:
  case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
    def isOrthogonal: Boolean = x1 == x2 || y1 == y2
    def points: Seq[(Int, Int)] =
      val xs = x1 to x2 by (if x1 < x2 then 1 else -1)
      val ys = y1 to y2 by (if y1 < y2 then 1 else -1)
      xs.zipAll(ys, x1, y1)

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Seq[Line] = lines.map: line =>
    val Array(x1, y1, x2, y2) = line.split("\\D+").map(_.toInt)
    Line(x1, y1, x2, y2)

object Solving:
  import DataDefs.*
  def overlap(lines: Seq[Line]): Int = lines
    .flatMap(_.points)
    .groupBy(identity)
    .count(_._2.length > 1)

  def solve1(lines: Seq[String]) = overlap(Parsing.parse(lines).filter(_.isOrthogonal))
  def solve2(lines: Seq[String]) = overlap(Parsing.parse(lines))

object Test:
  val file  = os.pwd / "2021" / "05" / "05.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 5
// Test.res2 // part 2: 12

object Main:
  val file  = os.pwd / "2021" / "05" / "05.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 5698
// Main.res2 // part 2: 15463
