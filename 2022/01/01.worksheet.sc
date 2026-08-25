object DataDefs:
  type Calories = Long
  case class Elf(snacks: Seq[Calories]):
    lazy val calories = snacks.sum

object Parsing:
  import DataDefs.*
  def parseElf(lines: String): Elf        = Elf(lines.split("\n").toSeq.map(_.toLong))
  def parseElves(lines: String): Seq[Elf] = lines.split("\n\n").toSeq.map(parseElf)

object Solving:
  import DataDefs.*
  def solve1(lines: String): Calories = Parsing
    .parseElves(lines)
    .map(_.calories)
    .max

  def solve2(lines: String): Calories = Parsing
    .parseElves(lines)
    .map(_.calories)
    .sorted
    .reverse
    .take(3)
    .sum

object Test:
  val file  = os.pwd / "2022" / "01" / "01.test.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 24000
// Test.res2 // part 2: 45000

object Main:
  val file  = os.pwd / "2022" / "01" / "01.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 71124
// Main.res2 // part 2: 204639
