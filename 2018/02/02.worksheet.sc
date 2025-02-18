object Solving:
  private def checkLine(line: String)(count: Int): Boolean =
    line.exists(char => line.count(_ == char) == count)

  private def differByOne(seq: Seq[String]): Boolean =
    seq(0).zip(seq(1)).count((c1, c2) => c1 != c2) == 1

  private def commonLetters(pair: (String, String)) =
    pair._1.zip(pair._2).filter(_ == _).map(_._1).mkString

  private def findCorrectBoxes(lines: Seq[String]) = lines
    .combinations(2)
    .filter(differByOne)
    .map(seq => (seq(0), seq(1)))
    .toList
    .head

  def solve1(lines: Seq[String]): Int =
    lines.count(checkLine(_)(2)) * lines.count(checkLine(_)(3))

  def solve2(lines: Seq[String]) = commonLetters(findCorrectBoxes(lines))

object Test:
  lazy val file1  = os.pwd / "2018" / "02" / "02.test.input.1.txt"
  lazy val file2  = os.pwd / "2018" / "02" / "02.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 12
// Test.res2 // part 2: fgij

object Main:
  lazy val file  = os.pwd / "2018" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 7350
// Main.res2 // part 2: wmlnjevbfodamyiqpucrhsukg
