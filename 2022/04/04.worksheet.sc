object DataDefs:
  case class AsgnPair(asgn1: Range, asgn2: Range):
    val fullyContains = asgn1.containsSlice(asgn2) || asgn2.containsSlice(asgn1)
    val overlaps      = asgn1.intersect(asgn2).nonEmpty

object Parsing:
  import DataDefs.*

  private def parsePair(line: String): AsgnPair = line match
    case s"$st1-$end1,$st2-$end2" =>
      AsgnPair(
        Range.inclusive(st1.toInt, end1.toInt),
        Range.inclusive(st2.toInt, end2.toInt)
      )

  def parsePairs(lines: Seq[String]): Seq[AsgnPair] = lines map parsePair

object Solving:
  import DataDefs.*

  def solve(pred: AsgnPair => Boolean)(lines: Seq[String]) = Parsing
    .parsePairs(lines)
    .count(pred)

  val solve1 = solve(_.fullyContains)
  val solve2 = solve(_.overlaps)

object Test:
  val file  = os.pwd / "2022" / "04" / "04.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 2
// Test.res2 // part 2: 4

object Main:
  val file  = os.pwd / "2022" / "04" / "04.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 526
// Main.res2 // part 2: 886
