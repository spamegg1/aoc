object DataDefs:
  type Pair = (BigInt, BigInt) // divisor, remainder

object Parsing:
  def parse1(line: String) = line.split(",").flatMap(_.toLongOption)
  def parse2(line: String) = line
    .split(",")
    .view
    .map(_.toIntOption)
    .zipWithIndex
    .filter(_._1.isDefined)
    .map((opt, ind) => (BigInt(opt.get), BigInt(-ind)))
    .toSeq

object Solving:
  import DataDefs.*

  def distance(target: Long)(bus: Long) = // part 1
    val quotient = target / bus
    bus * (quotient + 1L) - target

  def extendedEuclidean(a: BigInt, b: BigInt): Pair = // part 2
    var (oldRem, rem) = (a, b)
    var (oldS, s)     = (BigInt(1), BigInt(0))
    var (oldT, t)     = (BigInt(0), BigInt(1))
    var temp          = BigInt(0)

    while rem != BigInt(0) do
      val quotient = oldRem / rem

      temp = rem
      rem = oldRem - quotient * rem
      oldRem = temp

      temp = s
      s = oldS - quotient * s
      oldS = temp

      temp = t
      t = oldT - quotient * t
      oldT = temp

    (oldS, oldT) // Bezout coefficients

  def chineseRemainderOne(pair1: Pair, pair2: Pair): Pair = // part 2
    val (div1, rem1) = pair1
    val (div2, rem2) = pair2
    val (x, y)       = extendedEuclidean(div1, div2)
    var solution     = rem1 * y * div2 + rem2 * x * div1 // overflow here? use BigInt
    val newDiv       = div1 * div2
    (newDiv, solution)

  def chineseRemainder(pairs: Seq[Pair]) = // part 2
    pairs.foldLeft[Pair]((BigInt(1), BigInt(0)))(chineseRemainderOne)

  def solve1(line: String)(target: Long) = Parsing
    .parse1(line)
    .map(bus => bus -> distance(target)(bus))
    .minBy(_._2)
    .toList
    .product

  def solve2(line: String) =
    val (div, rem) = chineseRemainder(Parsing.parse2(line))
    val res        = rem % div
    res + (if res < 0 then div else 0)

object Test:
  lazy val file  = os.pwd / "2020" / "13" / "13.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines.head)(939L)
  lazy val res2  = lines map Solving.solve2
// Test.res1 // part 1: 59,5 = 295
// Test.res2 // part 2: 1068781,3417,754018,779210,1261476,1202161486

object Main:
  lazy val file = os.pwd / "2020" / "13" / "13.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)(1000390L)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 2298
// Main.res2 // part 2: 783685719679632
