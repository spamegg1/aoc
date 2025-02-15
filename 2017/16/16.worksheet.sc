object DataDefs:
  enum Move:
    case Spin(x: Int)
    case Exchange(a: Int, b: Int)
    case Partner(a: Char, b: Char)
  import Move.*

  extension (programs: Vector[Char])
    def process(move: Move) = move match
      case Spin(x) =>
        val fromTop = programs.size - x
        programs.drop(fromTop) ++ programs.take(fromTop)
      case Exchange(a, b) =>
        val (atA, atB) = (programs(a), programs(b))
        programs.updated(a, atB).updated(b, atA)
      case Partner(a, b) =>
        val (indA, indB) = (programs.indexOf(a), programs.indexOf(b))
        programs.updated(indA, b).updated(indB, a)

object Parsing:
  import DataDefs.*, Move.*

  def parseMove(move: String): Move = move match
    case s"s$x"    => Spin(x.toInt)
    case s"x$a/$b" => Exchange(a.toInt, b.toInt)
    case s"p$a/$b" => Partner(a.head, b.head)

  def parse(line: String): Seq[Move] = line.split(",").map(parseMove).toSeq

object Solving:
  import DataDefs.*

  def solve(using moves: Seq[Move])(input: String) = moves
    .foldLeft(input.toVector)((programs, move) => programs.process(move))
    .mkString

  private val memo = collection.mutable.Map[String, Int]()

  @annotation.tailrec
  private def findCycle(count: Int)(input: String)(using moves: Seq[Move]): (Int, Int) =
    val doMoves = solve(input)
    memo.get(doMoves) match
      case None =>
        memo += doMoves -> (count + 1)
        findCycle(count + 1)(doMoves)
      case Some(value) => (value, count + 1)

  def solve1(line: String) = solve(using Parsing.parse(line))
  def solve2(line: String)(input: String) =
    given Seq[Move]  = Parsing.parse(line)
    val (start, end) = findCycle(0)(input)
    val billion      = 1_000_000_000
    val index        = (billion - start) % (end - start) + start
    memo.find((string, int) => int == index).get._1

object Test:
  lazy val file = os.pwd / "2017" / "16" / "16.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)("abcde")
// Test.res1 // part 1: baedc

object Main:
  lazy val file = os.pwd / "2017" / "16" / "16.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)("abcdefghijklmnop")
  lazy val res2 = Solving.solve2(line)("abcdefghijklmnop")
// Main.res1 // part 1: hmefajngplkidocb
// Main.res2 // part 2: fbidepghmjklcnoa
