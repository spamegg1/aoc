package aoc2018.day05

object DataDefs:
  extension (left: Char)
    def react(right: Char): Boolean =
      (left.toLower == right.toLower) && (left.isLower ^ right.isLower)

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def helper(left: List[Char], right: List[Char]): Int = (left, right) match
    case (left, Nil)                                    => left.size
    case (left :: ls, right :: rs) if left.react(right) => helper(ls, rs)
    case (left, right :: rs)                            => helper(right :: left, rs)

  def shorten(line: String) = helper(Nil, line.toList)
  val solve1                = shorten

  def solve2(line: String): Int =
    val variations = for unitType <- 'a' to 'z' yield line.filter(_.toLower != unitType)
    variations.map(shorten).min

object Test:
  lazy val file = os.pwd / "2018" / "05" / "05.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

object Main:
  lazy val file = os.pwd / "2018" / "05" / "05.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 10
  println(Test.res2) // part 2: 4
  println(Main.res1) // part 1: 9348
  println(Main.res2) // part 2: 4996
