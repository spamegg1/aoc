object DataDefs:
  type Op    = Long => Long
  type Items = Seq[Long]

  case class Monkey(items: Items, op: Op, test: Int, yes: Int, no: Int, count: Long):
    def compose(g: Op)       = copy(op = op.andThen(g))
    def finish: Monkey       = copy(items = Seq(), count = count + items.size)
    def accept(extra: Items) = copy(items = items ++ extra)

object Parsing:
  import DataDefs.*

  def parseNums(line: String): Seq[Int] = line
    .split("\\D+")
    .tail
    .map(_.toInt)
    .toSeq

  def parseOp(line: String): Op = line
    .split(" ")
    .takeRight(2) match
    case Array("*", "old") => x => x * x
    case Array("*", y)     => x => x * y.toLong
    case Array("+", y)     => x => x + y.toLong

  def parse(lines: Seq[String]): Seq[Monkey] = lines
    .grouped(7)
    .toSeq
    .map: monkey =>
      val items = parseNums(monkey(1)).map(_.toLong)
      val op    = parseOp(monkey(2))
      val test  = parseNums(monkey(3)).head
      val yes   = parseNums(monkey(4)).head
      val no    = parseNums(monkey(5)).head
      Monkey(items, op, test, yes, no, 0)

object Solving:
  import DataDefs.*

  def step(monkeys: Seq[Monkey]): Seq[Monkey] = monkeys.indices
    .foldLeft(monkeys): (monkeys, index) =>
      val Monkey(items, op, test, yes, no, _) = monkeys(index)
      val (pass, fail) = items
        .map(op)
        .partition(_ % test == 0)
      monkeys
        .updated(index, monkeys(index).finish)
        .updated(yes, monkeys(yes).accept(pass))
        .updated(no, monkeys(no).accept(fail))

  def play(monkeys: Seq[Monkey], rounds: Int): Long = Iterator
    .iterate(monkeys)(step)
    .drop(rounds)
    .next()
    .map(_.count)
    .sorted
    .takeRight(2)
    .product

  def solve1(lines: Seq[String]) =
    val monkeys  = Parsing.parse(lines)
    val adjusted = monkeys.map(_.compose(_ / 3))
    play(adjusted, 20)

  def solve2(lines: Seq[String]) =
    val monkeys  = Parsing.parse(lines)
    val product  = monkeys.map(_.test).product
    val adjusted = monkeys.map(_.compose(_ % product))
    play(adjusted, 10000)

object Test:
  val file  = os.pwd / "2022" / "11" / "11.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 10605
// Test.res2 // part 2: 2713310158

object Main:
  val file  = os.pwd / "2022" / "11" / "11.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 58786
// Main.res2 // part 2: 14952185856
