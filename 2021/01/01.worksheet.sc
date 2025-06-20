object Parsing:
  def parse(lines: Seq[String]) = lines.map(_.toInt)

object Solving:
  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .sliding(2)
    .foldLeft(0)((acc, list) => acc + (if list(0) < list(1) then 1 else 0))

  def solve2(lines: Seq[String]) = Parsing
    .parse(lines)
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .foldLeft(0)((acc, list) => acc + (if list(0) < list(1) then 1 else 0))

object Test:
  lazy val file  = os.pwd / "2021" / "01" / "01.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 7
// Test.res2 // part 2: 5

object Main:
  lazy val file  = os.pwd / "2021" / "01" / "01.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1692
// Main.res2 // part 2: 1724
