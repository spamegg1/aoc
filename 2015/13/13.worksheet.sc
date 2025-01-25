object Parsing:
  private def parseLine(line: String): (Seq[String], Long) = line match
    case s"$p1 would gain $score happiness units by sitting next to $p2." =>
      Seq(p1, p2) -> score.toLong
    case s"$p1 would lose $score happiness units by sitting next to $p2." =>
      Seq(p1, p2) -> -score.toLong

  def parseGuests(lines: Seq[String])    = lines.map(_.split(" ").head).distinct
  def parseHappiness(lines: Seq[String]) = lines.map(parseLine).toMap.withDefaultValue(0L)

object Solving:
  private def totalHappiness(levels: Map[Seq[String], Long])(guests: Seq[String]) =
    guests
      .sliding(2)
      .map(pair => levels(pair) + levels(pair.reverse))
      .sum

  private def solve(guests: Seq[String])(lines: Seq[String]) =
    val seatingArrangements = guests.permutations.map(a => a.appended(a.head))
    val happinessLevels     = Parsing.parseHappiness(lines)
    seatingArrangements.map(totalHappiness(happinessLevels)).max

  def solve1(lines: Seq[String]) = solve(Parsing.parseGuests(lines))(lines)
  def solve2(lines: Seq[String]) = solve(Parsing.parseGuests(lines).appended("me"))(lines)

object Test:
  lazy val file  = os.pwd / "2015" / "13" / "13.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 330
// Test.res2 // part 2: 286

object Main:
  lazy val file  = os.pwd / "2015" / "13" / "13.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 733
// Main.res2 // part 2: 725
