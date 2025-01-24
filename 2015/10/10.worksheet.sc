object DataDefs:
  lazy val file     = os.pwd / "2015" / "10" / "10.table.txt"
  lazy val tableRaw = os.read.lines(file)

  type Name     = String
  type Sequence = String
  type Table    = Map[Name, Element]

  case class Element(name: Name, seq: Sequence, decays: List[Name])

  case class Compound(elements: List[Name])(using table: Table):
    lazy val decaysTo = Compound(elements.flatMap(table(_).decays))
    lazy val size     = elements.map(table(_).seq.size).sum

object Parsing:
  import DataDefs.*

  def parseElement(line: String): Element =
    val parts = line.split(" ")
    Element(parts(1), parts(2), parts(3).split('.').toList)

  def parseTable(lines: Seq[String]): Table =
    lines.map(parseElement).map(e => e.name -> e).toMap

object Solving:
  import DataDefs.*
  given Table           = Parsing.parseTable(tableRaw)
  given start: Compound = Compound(List("Yb"))

  @annotation.tailrec
  def decayMany(compound: Compound)(times: Int): Int =
    if times == 0 then compound.size
    else decayMany(compound.decaysTo)(times - 1)

  val solve = decayMany(start)

object Main:
  lazy val res1 = Solving.solve(40)
  lazy val res2 = Solving.solve(50)
// Main.res1 // part 1: 492982
// Main.res2 // part 2: 6989950
