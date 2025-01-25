object Parsing:
  // combinations removes repetitions, so we zipWithIndex to make them distinct.
  def parse(lines: Seq[String]) = lines.map(_.toInt).zipWithIndex

object Solving:
  def powerSet(containers: Seq[(Int, Int)]) = // brute force attempt
    (0 until containers.size).flatMap(containers.combinations(_))

  def solve1(lines: Seq[String])(eggnog: Int) =
    val containers = Parsing.parse(lines)
    powerSet(containers).count(set => set.map(_._1).sum == eggnog)

  def solve2(lines: Seq[String])(eggnog: Int) =
    val containers = Parsing.parse(lines)
    val good       = powerSet(containers).filter(set => set.map(_._1).sum == eggnog)
    val minSize    = good.minBy(_.size).size
    good.count(_.size == minSize)

object Test:
  lazy val file  = os.pwd / "2015" / "17" / "17.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(25)
  lazy val res2  = Solving.solve2(lines)(25)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 3

object Main:
  lazy val file  = os.pwd / "2015" / "17" / "17.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(150)
  lazy val res2  = Solving.solve2(lines)(150)
// Main.res1 // part 1: 654
// Main.res2 // part 2: 57
