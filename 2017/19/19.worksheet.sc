object DataDefs:
  enum Dir:
    case N, S, E, W
  import Dir.*

  case class Packet(dir: Dir, cur: Char, row: Int, col: Int, seen: List[Char]):
    def allSeen              = seen.reverse.mkString
    def done(target: String) = target.toSet == seen.toSet
    def next = dir match
      case N =>
      case S =>
      case E =>
      case W =>

object Parsing:
  import DataDefs.*

  def parseLine(line: String)   = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(start: (Int, Int))(target: String) = 0L
  def solve2(lines: Seq[String])(start: (Int, Int))(target: String) = 0L

object Test:
  lazy val file   = os.pwd / "2017" / "19" / "19.test.input.txt"
  lazy val lines  = os.read.lines(file)
  lazy val start  = (0, 5)
  lazy val target = "ABCDEF"
  lazy val res1   = Solving.solve1(lines)(start)(target)
  lazy val res2   = Solving.solve2(lines)(start)(target)
// Test.res1 // part 1: ABCDEF
// Test.res2 // part 2:

object Main:
  lazy val file   = os.pwd / "2017" / "19" / "19.input.txt"
  lazy val lines  = os.read.lines(file)
  lazy val start  = (0, 115)
  lazy val target = "ABFHMPTYZ"
  lazy val res1   = Solving.solve1(lines)(start)(target)
  lazy val res2   = Solving.solve2(lines)(start)(target)
// Main.res1 // part 1: PBAZYFMHT
// Main.res2 // part 2: 16072
