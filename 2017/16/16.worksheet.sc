/*
--- Day 16: Permutation Promenade ---
You come upon a very unusual sight; a group of programs here appear to be dancing.
There are sixteen programs in total, named a through p.
They start by standing in a line: a stands in position 0,
b stands in position 1, and so on until p, which stands in position 15.

The programs' dance consists of a sequence of dance moves:
  Spin, written sX, makes X programs move from the end to the front,
    but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
  Exchange, written xA/B, makes the programs at positions A and B swap places.
  Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde),
they could do the following dance:
  s1, a spin of size 1: eabcd.
  x3/4, swapping the last two programs: eabdc.
  pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.
You watch the dance for a while and record their dance moves (your puzzle input).
In what order are the programs standing after their dance?

--- Part Two ---
Now that you're starting to get a feel for the dance moves,
you turn your attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance,
the programs perform it again and again: including the first dance,
a total of one billion (1000000000) times.

In the example above, their second dance would begin with the order baedc,
and use the same dance moves:
  s1, a spin of size 1: cbaed.
  x3/4, swapping the last two programs: cbade.
  pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?
 */
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
    given Seq[Move] = Parsing.parse(line)
    val (start, end) = findCycle(0)(input)
    val billion = 1_000_000_000
    val index = (billion - start) % (end - start) + start
    memo.find((string, int) => int == index).get._1

object Testing:
  private lazy val line = os.read.lines(os.pwd / "2017" / "16" / "16.test.input.txt").head
  lazy val result1 = Solving.solve1(line)("abcde")
// Testing.result1 // part 1: baedc

object Main:
  private lazy val line = os.read.lines(os.pwd / "2017" / "16" / "16.input.txt").head
  lazy val result1 = Solving.solve1(line)("abcdefghijklmnop")
  lazy val result2 = Solving.solve2(line)("abcdefghijklmnop")
// Main.result1 // part 1: hmefajngplkidocb
// Main.result2 // part 2: fbidepghmjklcnoa
