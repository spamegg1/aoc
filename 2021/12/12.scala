package aoc2021.day12

object DataDefs:
  type Node = String
  type Path = List[Node]
  type Rule = (Node, Path) => Boolean
  type Cave = Map[Node, Path]

object Parsing:
  import DataDefs.*

  def parseLine(line: String): (Node, Node) =
    val split = line.split("-")
    (split.head, split.last)

  def parse(lines: Seq[String]): Cave = lines
    .foldLeft[Cave](Map().withDefaultValue(Nil)):
      case (cave, line) =>
        val (start, end) = parseLine(line)
        cave
          .updated(start, end :: cave(start))
          .updated(end, start :: cave(end))

object Solving:
  import DataDefs.*

  def small(name: String): Boolean = name.head.isLower

  def helper(current: String, path: Path)(using cave: Cave, rule: Rule): Int =
    cave(current).foldLeft(0):
      case (total, next) =>
        if next == "end" then total + 1
        else if next == "start" then total
        else if small(next) && rule(next, path.filter(small)) then total
        else total + helper(next, next :: path)

  def descend(cave: Cave)(rule: Rule): Int = helper("start", Nil)(using cave, rule)

  def solve1(lines: Seq[String]) =
    descend(Parsing.parse(lines)): (node, path) =>
      path.contains(node)

  def solve2(lines: Seq[String]) =
    descend(Parsing.parse(lines)): (next, path) =>
      val freqs   = (next :: path).groupMapReduce(identity)(_ => 1)(_ + _)
      val lengths = freqs.values
      lengths.exists(_ > 2) || lengths.count(_ == 2) > 1

object Test:
  val path  = os.pwd / "2021" / "12"
  val file1 = path / "12.test.input.1.txt"
  val file2 = path / "12.test.input.2.txt"
  val file3 = path / "12.test.input.3.txt"
  val lines = Seq(file1, file2, file3).map(os.read.lines)
  val res1  = lines map Solving.solve1
  val res2  = lines map Solving.solve2

object Main:
  val file  = os.pwd / "2021" / "12" / "12.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 10, 19, 226
  println(Test.res2) // part 2: 36, 103, 3509
  println(Main.res1) // part 1: 3761
  println(Main.res2) // part 2: 99138
