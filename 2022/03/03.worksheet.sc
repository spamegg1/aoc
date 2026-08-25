object DataDefs:
  val Priorities = (('a' to 'z').zip(1 to 26) ++ ('A' to 'Z').zip(27 to 52)).toMap
  type Compartment = String

  case class Rucksack(stuff: String):
    val halfSize   = stuff.length / 2
    val first      = stuff.take(halfSize)
    val second     = stuff.drop(halfSize)
    val findCommon = first.toSet.intersect(second.toSet).headOption
    val priority   = findCommon.map(char => Priorities(char)) // part 1

  case class Group(rucksacks: Seq[Rucksack]):
    val first  = rucksacks.head
    val second = rucksacks.tail.head
    val third  = rucksacks.last
    val findCommon = first.stuff.toSet
      .intersect(second.stuff.toSet)
      .intersect(third.stuff.toSet)
      .headOption
    val priority = findCommon.map(char => Priorities(char))

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Rucksack        = Rucksack(line)
  def parse(lines: Seq[String]): Seq[Rucksack] = lines map parseLine // part 1

  def parseGroups(lines: Seq[String]): Seq[Group] = parse(lines) // part 2
    .grouped(3)
    .map(Group(_))
    .toSeq

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .flatMap(_.priority)
    .sum

  def solve2(lines: Seq[String]) = Parsing
    .parseGroups(lines)
    .flatMap(_.priority)
    .sum

object Test:
  val file  = os.pwd / "2022" / "03" / "03.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 157
// Test.res2 // part 2: 70

object Main:
  val file  = os.pwd / "2022" / "03" / "03.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 7446
// Main.res2 // part 2: 2646
