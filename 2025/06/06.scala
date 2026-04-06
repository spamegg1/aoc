package aoc2025.day06

object DataDefs:
  type Op = (Long, Long) => Long

  case class Calc(nums: Seq[Long], op: Op, zero: Long):
    def calc: Long = nums.foldLeft(zero)(op)

  extension (s: String)
    def toOp: Op = s match
      case "+" => _ + _
      case "*" => _ * _
    def toZero: Long = s match
      case "+" => 0L
      case "*" => 1L

object Parsing:
  import DataDefs.*

  def parseNums(line: String)  = line.trim.split(" +").toSeq.map(_.toLong)
  def parseOps(line: String)   = line.trim.split(" +").toSeq.map(_.toOp)
  def parseZeros(line: String) = line.trim.split(" +").toSeq.map(_.toZero)

  def parse(lines: Seq[String]): Seq[Calc] =
    val (numbers, ops) = (lines.init, lines.last)
    val parsedNums     = numbers map parseNums
    val parsedOps      = parseOps(ops)
    val parsedZeros    = parseZeros(ops)
    for
      index <- 0 until parsedOps.size
      nums = parsedNums.map(seq => seq(index))
      op   = parsedOps(index)
      zero = parsedZeros(index)
    yield Calc(nums, op, zero)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing.parse(lines).map(_.calc).sum

  def solve2(lines: Seq[String]) = 0L

object Test:
  val file  = os.pwd / "2025" / "06" / "06.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2025" / "06" / "06.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 4277556
  println(Test.res2) // part 2:
  println(Main.res1) // part 1: 5782351442566
  println(Main.res2) // part 2:
