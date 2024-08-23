/*
--- Day 10: Elves Look, Elves Say ---
Today, the Elves are playing a game called look-and-say.
They take turns making sequences by reading aloud the previous
sequence and using that reading as the next sequence.
For example, 211 is read as "one two, two ones",
which becomes 1221 (1 2, 2 1s).

Look-and-say sequences are generated iteratively,
using the previous value as input for the next step.
For each step, take the previous value, and
replace each run of digits (like 111) with the number of digits (3)
followed by the digit itself (1).

For example:
  1 becomes 11 (1 copy of digit 1).
  11 becomes 21 (2 copies of digit 1).
  21 becomes 1211 (one 2 followed by one 1).
  1211 becomes 111221 (one 1, one 2, and two 1s).
  111221 becomes 312211 (three 1s, two 2s, and one 1).

Starting with the digits in your puzzle input, apply this process 40 times.
What is the length of the result?

--- Part Two ---
Neat, right? You might also enjoy hearing John Conway talking about this sequence
(that's Conway of Conway's Game of Life fame).

Now, starting again with the digits in your puzzle input,
apply this process 50 times. What is the length of the new result?
 */
object DataDefs:
  lazy val tableRaw = os.read.lines(os.pwd / "10.table.txt")
  type Name = String
  type Sequence = String
  type Table = Map[Name, Element]

  case class Element(name: Name, seq: Sequence, decays: List[Name])
  case class Compound(elements: List[Name])(using table: Table):
    lazy val decaysTo = Compound(elements.flatMap(table(_).decays))
    lazy val size = elements.map(table(_).seq.size).sum

object Parsing:
  import DataDefs.*

  def parseElement(line: String): Element =
    val parts = line.split(" ")
    Element(parts(1), parts(2), parts(3).split('.').toList)

  def parseTable(lines: Seq[String]): Table =
    lines.map(parseElement).map(e => e.name -> e).toMap

object Solving:
  import DataDefs.*
  given table: Table = Parsing.parseTable(tableRaw)
  given start: Compound = Compound(List("Yb"))

  @annotation.tailrec
  def decayMany(compound: Compound)(times: Int): Int =
    if times == 0 then compound.size
    else decayMany(compound.decaysTo)(times - 1)

  val solve = decayMany(start)

object Main:
  lazy val result1 = Solving.solve(40)
  lazy val result2 = Solving.solve(50)
// Main.result1 // part 1: 492982
// Main.result2 // part 2: 6989950
