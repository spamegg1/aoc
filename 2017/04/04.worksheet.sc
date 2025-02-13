object DataDefs:
  case class Passphrase(words: Array[String]):
    lazy val isValid = words.distinct.size == words.size // part 1
    lazy val noAnagrams = words.view
      .map(_.sorted)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .forall(_._2 == 1)

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Passphrase        = Passphrase(line.split(" "))
  def parse(lines: Seq[String]): Seq[Passphrase] = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]): Int = Parsing.parse(lines).count(_.isValid)
  def solve2(lines: Seq[String]): Int = Parsing.parse(lines).count(_.noAnagrams)

object Test:
  lazy val file  = os.pwd / "2017" / "04" / "04.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 7
// Test.res2 // part 2: 5

object Main:
  lazy val file  = os.pwd / "2017" / "04" / "04.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 455
// Main.res2 // part 2: 186
