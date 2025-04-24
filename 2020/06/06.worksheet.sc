object DataDefs:
  type Group     = List[String]
  type Groups    = List[Group]
  type Predicate = Group => (String => Boolean) => Boolean

object Parsing:
  import DataDefs.*

  private def parseOneGroup(lines: Group): (Group, Group) =
    (lines.takeWhile(_.nonEmpty), lines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty))

  private def parseGroups(lines: Group)(groups: Groups): Groups = lines match
    case Nil => groups
    case _ =>
      val (group, rest) = parseOneGroup(lines)
      parseGroups(rest)(group :: groups)

  def parse(lines: Group): Groups = parseGroups(lines)(Nil)

object Solving:
  import DataDefs.*

  private def solve(pred: Predicate)(lines: Group) = Parsing
    .parse(lines)
    .map(group => ('a' to 'z').count(char => pred(group)(_.contains(char))))
    .sum

  lazy val solve1 = solve(_.exists)
  lazy val solve2 = solve(_.forall)

object Test:
  lazy val file  = os.pwd / "2020" / "06" / "06.test.input.txt"
  lazy val lines = os.read.lines(file).toList
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 11
// Test.res2 // part 2: 6

object Main:
  lazy val file  = os.pwd / "2020" / "06" / "06.input.txt"
  lazy val lines = os.read.lines(file).toList
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 6680
// Main.res2 // part 2: 3117
