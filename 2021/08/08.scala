package aoc2021.day08

object DataDefs:
  val segments = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g')
  // Map *sorted* non-scrambled segment pattern to digit
  val segmentsToDigit = Seq(
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
  ).zipWithIndex.toMap

  type Pat = String
  extension (p: Pat)
    def isKnownDigit: Boolean = p.length match
      case 2 | 3 | 4 | 7 => true
      case _             => false

  case class Entry(sigs: Seq[Pat], outs: Seq[Pat]):
    def fromLength(n: Int) = sigs.filter(_.length == n).flatten.toSet
    def unscramble: Int =
      // Digit:  0 1 2 3 4 5 6 7 8 9
      // Length: 6 2 5 5 4 5 6 3 7 6
      // Segment:     a b c d e f g
      // Occurrences: 8 6 8 7 4 9 7
      val occurrences = sigs.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      def fromOccurrences(n: Int) = segments
        .filter: char =>
          occurrences(char) == n
        .toSet

      // Digit 1 has pattern length two and uses segments c and f
      val setCF   = fromLength(2)
      val setBCDF = fromLength(4)

      val setE  = fromOccurrences(4) // Whichever letter is mapped to e will occur 4 times
      val setB  = fromOccurrences(6)
      val setDG = fromOccurrences(7) // Both letters mapped to d and g occur 7 times each
      val setAC = fromOccurrences(8)
      val setF  = fromOccurrences(9)

      val setC = setCF.diff(setF) // Set difference eliminates the unknowns
      val setA = setAC.diff(setC)
      val setG = setDG.diff(setBCDF)
      val setD = setDG.diff(setG)

      val unscrambled = Seq(setA, setB, setC, setD, setE, setF, setG)
        .zip(segments)
        .map((k, v) => (k.head, v))
        .toMap

      outs
        .map: scrambled =>
          segmentsToDigit(scrambled.map(unscrambled).sorted)
        .mkString
        .toInt
    end unscramble

object Parsing:
  import DataDefs.*

  def parseEntry(line: String): Entry =
    val Array(sigs, outs) = line.split(" \\| ")
    Entry(sigs.split(" ").toSeq, outs.split(" ").toSeq)

  def parse(lines: Seq[String]) = lines map parseEntry

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(_.outs.count(_.isKnownDigit))
    .sum

  def solve2(lines: Seq[String]): Int = Parsing
    .parse(lines)
    .map(_.unscramble)
    .sum

object Test:
  val file  = os.pwd / "2021" / "08" / "08.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2021" / "08" / "08.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 26
  println(Test.res2) // part 2: 61229
  println(Main.res1) // part 1: 554
  println(Main.res2) // part 2: 990964
