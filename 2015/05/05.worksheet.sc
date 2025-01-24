object DataDefs:
  extension (s: String)
    def containsThreeVowels: Boolean  = s.count("aeiou".contains) >= 3
    def containsDoubleLetter: Boolean = s.sliding(2).exists(p => p(0) == p(1))

    def doesNotContain: Boolean =
      !s.contains("ab") && !s.contains("cd") &&
        !s.contains("pq") && !s.contains("xy")

    def isNice1: Boolean =
      doesNotContain && containsDoubleLetter && containsThreeVowels

    def containsDoublePair: Boolean =
      val pairs = s.sliding(2).toList
      pairs.exists: p =>
        val first  = s.indexOf(p)
        val second = s.drop(first + 2).indexOf(p)
        second != -1

    def containsXyx: Boolean = s.sliding(3).exists(p => p(0) == p(2))
    def isNice2: Boolean     = containsDoublePair && containsXyx

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = lines.count(_.isNice1)
  def solve2(lines: Seq[String]) = lines.count(_.isNice2)

object Test:
  lazy val file1  = os.pwd / "2015" / "05" / "05.test.input.txt"
  lazy val file2  = os.pwd / "2015" / "05" / "05.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 2
// Test.res2 // part 2: 2

object Main:
  lazy val file  = os.pwd / "2015" / "05" / "05.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 255
// Main.res2 // part 2: 55
