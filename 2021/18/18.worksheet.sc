object DataDefs:
  type Node    = (value: Int, depth: Int)
  type Number  = Seq[Node]
  type Numbers = Seq[Number]

  extension (node: Node)
    def expl(number: Number) =
      val (left, right) = number.splitAt(number.indexOf(node) + 1)
      val nextLeft = left match
        case rest :+ prev :+ lft => rest.appended((prev.value + lft.value, prev.depth))
        case _                   => Seq()
      val nextRight = right match
        case rgt +: next +: rest => rest.prepended((next.value + rgt.value, next.depth))
        case _                   => Seq()
      nextLeft ++ Seq((0, 4)) ++ nextRight

  extension (number: Number)
    def explode: Option[Number] = number
      .find(_.depth == 5)
      .map(_.expl(number))

    def split: Option[Number] = number
      .find(_.value >= 10)
      .map: node =>
        val node1 = (node.value / 2, node.depth + 1)
        val node2 = (node.value / 2 + node.value % 2, node.depth + 1)
        val slice = Seq(node1, node2)
        number.patch(number.indexOf(node), slice, 1)

    def magnitude: Int =
      def helper(num: Number): Number = num match
        case Seq(left, right, rest*) if left.depth == right.depth =>
          val merged = (3 * left.value + 2 * right.value, left.depth - 1)
          helper(helper(rest).prepended(merged))
        case other => other
      helper(number).head.value
  end extension

  object Number:
    def add(left: Number, right: Number): Number =
      def helper(num: Number): Number =
        explode(num).orElse(split(num)) match
          case Some(next) => helper(next)
          case None       => num
      helper((left ++ right).map(node => (node.value, node.depth + 1)))
end DataDefs

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Number =
    val (_, num) = line.foldLeft(0 -> Seq.empty[Node]):
      case ((depth, number), next) =>
        next match
          case '[' => (depth + 1) -> number
          case ']' => (depth - 1) -> number
          case ',' => depth       -> number
          case _   => depth       -> number.appended((next.asDigit, depth))
    num

  def parse(lines: Seq[String]) = lines map parseLine
end Parsing

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .reduce(Number.add)
    .magnitude

  def solve2(lines: Seq[String]) =
    val numbers = Parsing.parse(lines)
    val combinations =
      for
        first  <- numbers
        second <- numbers
        if first != second
      yield Number.add(first, second)
    combinations.map(_.magnitude).max

object Test:
  val file  = os.pwd / "2021" / "18" / "18.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 4140
// Test.res2 // part 2: 3993

object Main:
  val file  = os.pwd / "2021" / "18" / "18.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 4176
// Main.res2 // part 2: 4633
