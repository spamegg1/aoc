package aoc2018.day01

object DataDefs:
  type Freq = Long

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = lines.map(_.toLong).toList

object Solving:
  import DataDefs.*

  def traverse(master: List[Freq]): Freq =
    @annotation.tailrec
    def helper(freqs: List[Freq])(total: Freq)(seen: Set[Freq]): Freq = freqs match
      case head :: next =>
        val newTotal = total + head
        if seen contains newTotal then newTotal
        else helper(next)(newTotal)(seen + newTotal)
      case Nil => helper(master)(total)(seen)
    helper(master)(0L)(Set(0L))

  def solve1(lines: Seq[String]): Freq = Parsing.parse(lines).sum
  def solve2(lines: Seq[String]): Freq = traverse(Parsing.parse(lines))

object Test:
  lazy val file1  = os.pwd / "2018" / "01" / "01.test.input.1.txt"
  lazy val file2  = os.pwd / "2018" / "01" / "01.test.input.2.txt"
  lazy val file3  = os.pwd / "2018" / "01" / "01.test.input.3.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val lines3 = os.read.lines(file3)
  lazy val res    = Seq(lines1, lines2, lines3) map Solving.solve2

object Main:
  lazy val file  = os.pwd / "2018" / "01" / "01.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res)  // part 1: 2, 10, 5
  println(Main.res1) // part 1: 442
  println(Main.res2) // part 2: 59908, very fast using Set instead of List (.contains)
