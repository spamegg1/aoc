object DataDefs:
  type Color = String
  type Bag   = (color: Color, count: Long)
  type Bags  = Seq[Bag]
  type Rule  = (Color, Bags)
  type Rules = Map[Color, Bags]

  extension (color: Color)
    def canContainShiny(using rules: Rules): Boolean =
      val colors = rules(color).map(_.color)
      colors.exists(_ == "shiny gold") || colors.exists(_.canContainShiny)

    def totalBags(using rules: Rules): Long =
      rules(color).map(bag => bag.count * bag.color.totalBags).sum + 1

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Rule = line match
    case s"$color bag contain no other bag." => color -> Seq()
    case s"$color bag contain $branches."    => color -> parseBranches(branches)

  def parseBranches(branches: String) = branches.split(", ").map(parseOne).toSeq

  def parseOne(branch: String): Bag = branch match
    case s"$count $color bag" => (color, count.toLong)

  def parse(lines: Seq[String]): Rules = lines.map(parseLine).toMap

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    given rules: Rules = Parsing.parse(lines)
    rules.keys.count(_.canContainShiny)

  def solve2(lines: Seq[String]) =
    given rules: Rules = Parsing.parse(lines)
    "shiny gold".totalBags - 1

object Test:
  lazy val file1  = os.pwd / "2020" / "07" / "07.test.input.txt"
  lazy val file2  = os.pwd / "2020" / "07" / "07.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 126

object Main:
  lazy val file  = os.pwd / "2020" / "07" / "07.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 268
// Main.res2 // part 2: 7867
