object DataDefs:
  case class Column(chars: Seq[Char]):
    private lazy val freqs = chars.groupMapReduce(identity)(_ => 1)(_ + _)
    lazy val mostCommon    = freqs.maxBy(_._2)._1
    lazy val leastCommon   = freqs.minBy(_._2)._1

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Seq[Column] =
    val size = lines.head.length
    for index <- 0 until size
    yield Column(for line <- lines yield line(index))

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(_.mostCommon)
    .mkString

  def solve2(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(_.leastCommon)
    .mkString

object Test:
  lazy val file  = os.pwd / "2016" / "06" / "06.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: easter
// Test.res2 // part 2: advent

object Main:
  lazy val file  = os.pwd / "2016" / "06" / "06.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: qqqluigu
// Main.res2 // part 2: lsoypmia
