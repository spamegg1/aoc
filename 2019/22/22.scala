package aoc2019.day22

object DataDefs:
  case class Technique(a: BigInt, c: BigInt, m: BigInt):
    def mod(n: BigInt)             = n % m
    def shuffle(index: Long): Long = mod(a * index + c).toLong

    infix def merge(other: Technique): Technique =
      val nextA = mod(a * other.a)
      val nextC = mod(c * other.a + other.c)
      Technique(nextA, nextC, m)

    def inverse: Technique =
      val nextA = a.modInverse(m)
      val nextC = mod(nextA * -c)
      Technique(nextA, nextC, m)

    def pow(exp: BigInt): Technique =
      val nextA = a.modPow(exp, m)
      val nextC = mod((nextA - 1) * (a - 1).modInverse(m) * c)
      Technique(nextA, nextC, m)

object Parsing:
  import DataDefs.*

  def parseLine(size: Long)(line: String): Technique = line match
    case s"deal into new stack"    => Technique(size - 1, size - 1, size)
    case s"deal with increment $n" => Technique(n.toLong, 0, size)
    case s"cut $n"                 => Technique(1, size - n.toLong, size)

  def parse(lines: Seq[String])(size: Long): Technique = lines
    .map(parseLine(size))
    .reduce(_ merge _)

object Solving:
  def solve1(lines: Seq[String])(deckSize: Long)(card: Int) = Parsing
    .parse(lines)(deckSize)
    .shuffle(card)

  def solve2(lines: Seq[String])(deckSize: Long)(repeat: Long)(card: Int) = Parsing
    .parse(lines)(deckSize)
    .inverse
    .pow(repeat)
    .shuffle(card)

object Main:
  lazy val file  = os.pwd / "2019" / "22" / "22.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(10007)(2019)
  lazy val res2  = Solving.solve2(lines)(119315717514047L)(101741582076661L)(2020)

@main
def run: Unit =
  println(Main.res1) // part 1: 6417
  println(Main.res2) // part 2: 98461321956136
