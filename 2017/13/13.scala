package aoc2017.day13

object DataDefs:
  type Pico     = Int
  type Depth    = Int
  type Range    = Int
  type Severity = Int
  type Firewall = Seq[(Depth, Range)]

  extension (f: Firewall)
    def severity(delay: Pico): Seq[Severity] = f.flatMap: (depth, range) =>
      val caught = (depth + delay) % (2 * range - 2) == 0
      Option.when(caught)(depth * range)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): (Depth, Range) = line match
    case s"$depth: $range" => depth.toInt -> range.toInt

  def parse(lines: Seq[String]): Firewall = lines.map(parseLine)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing.parse(lines).severity(delay = 0).sum

  def solve2(lines: Seq[String]) =
    val firewall = Parsing.parse(lines)
    Iterator
      .from(0)
      .indexWhere(firewall.severity(_).isEmpty)

object Test:
  lazy val file  = os.pwd / "2017" / "13" / "13.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2017" / "13" / "13.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 24
  println(Test.res2) // part 2: 10
  println(Main.res1) // part 1: 2688
  println(Main.res2) // part 2: 3876272
