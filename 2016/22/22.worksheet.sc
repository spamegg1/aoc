object DataDefs:
  case class Node(row: Int, col: Int, size: Int, used: Int, avail: Int, use: Int)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Node = line match
    case s"/dev/grid/node-x$row-y$col ${size}T ${used}T ${avail}T ${use}%" =>
      Node(row.toInt, col.toInt, size.toInt, used.toInt, avail.toInt, use.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def viable(a: Node, b: Node): Boolean = a.used != 0 && a != b && a.used <= b.avail

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .combinations(2)
    .count(seq => viable(seq.head, seq.last) || viable(seq.last, seq.head))

  def solve2(lines: Seq[String]) = 0L

object Test:
  lazy val file  = os.pwd / "2016" / "22" / "22.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 2: 7
// Test.res2 // part 2: 7

object Main:
  lazy val file  = os.pwd / "2016" / "22" / "22.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 981
// Main.res2 // part 2: 233
