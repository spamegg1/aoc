object DataDefs:
  type Item  = (String, Int)
  type Items = Map[String, Int]
  case class Sue(id: Int, items: Items)
  type Memory = Map[String, Int]
  type Check  = Memory => Sue => Iterable[Boolean]

object Parsing:
  import DataDefs.*

  private def parseItem(line: String): Item = line match
    case s"$item: $count" => item -> count.toInt

  private def parseItems(line: String): Items = line.split(", ").map(parseItem).toMap

  private def parseSue(line: String): Sue = line match
    case s"Sue $id: $items" => Sue(id.toInt, parseItems(items))

  def parseMemory(lines: Seq[String]): Memory = lines.map(parseItem).toMap
  def parseSues(lines: Seq[String]): Seq[Sue] = lines map parseSue

object Solving:
  import DataDefs.*

  private def checkSue1(memory: Memory)(sue: Sue) =
    for (item, count) <- sue.items if memory.isDefinedAt(item)
    yield count == memory(item)

  private def checkSue2(memory: Memory)(sue: Sue) =
    for (item, count) <- sue.items if memory.isDefinedAt(item)
    yield item match
      case "cats" | "trees"           => count > memory(item)
      case "pomeranians" | "goldfish" => count < memory(item)
      case _                          => count == memory(item)

  private def solve(check: Check)(lines: Seq[String])(memories: Seq[String]) =
    val memory = Parsing.parseMemory(memories)
    Parsing
      .parseSues(lines)
      .find(sue => check(memory)(sue).forall(_ == true))
      .get
      .id

  val solve1 = solve(checkSue1)
  val solve2 = solve(checkSue2)

object Main:
  lazy val file1 = os.pwd / "2015" / "16" / "16.test.input.txt"
  lazy val file2 = os.pwd / "2015" / "16" / "16.input.txt"
  lazy val mems  = os.read.lines(file1)
  lazy val lines = os.read.lines(file2)
  lazy val res1  = Solving.solve1(lines)(mems)
  lazy val res2  = Solving.solve2(lines)(mems)
// Main.res1 // part 1: 213
// Main.res2 // part 2: 323
