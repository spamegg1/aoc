object Parsing:
  def parse(lines: Seq[String]) = lines.map(_.split("\t").map(_.toInt).toSeq)

object Solving:
  def solve1(lines: Seq[String]): Int = Parsing
    .parse(lines)
    .map(line => line.max - line.min)
    .sum

  private def evenlyDivides(seq: Seq[Int]): Int =
    val (smaller, bigger) = (seq(0), seq(1))
    val decimal           = bigger.toDouble / smaller.toDouble
    val int               = bigger / smaller
    if decimal == int then int else 0

  def solve2(lines: Seq[String]): Int = Parsing
    .parse(lines)
    .flatMap(_.sorted.combinations(2).map(evenlyDivides))
    .sum

object Test:
  lazy val file1  = os.pwd / "2017" / "02" / "02.test.input.txt"
  lazy val file2  = os.pwd / "2017" / "02" / "02.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 18
// Test.res2 // part 2: 9

object Main:
  lazy val file  = os.pwd / "2017" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 32121
// Main.res2 // part 2: 197
