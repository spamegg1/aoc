import collection.mutable.{Map => MMap}

object DataDefs:
  enum Op:
    case Add, Sub, Mul, Div

  type Monkey = String
  enum Tree:
    case Leaf(value: Long)
    case Branch(left: Monkey, right: Monkey, op: Op)
  import Tree.*

  type Monkeys = Map[Monkey, Tree]
  type Yell    = Long
  type Yells   = MMap[Monkey, Yell]

object Parsing:
  import DataDefs.*, Op.*, Tree.*

  def parseMonkey(line: String): (Monkey, Tree) = line match
    case s"$monkey: $left + $right" => monkey -> Branch(left, right, Add)
    case s"$monkey: $left - $right" => monkey -> Branch(left, right, Sub)
    case s"$monkey: $left * $right" => monkey -> Branch(left, right, Mul)
    case s"$monkey: $left / $right" => monkey -> Branch(left, right, Div)
    case s"$monkey: $value"         => monkey -> Leaf(value.toLong)

  def parse(lines: Seq[String]): Monkeys = lines.map(parseMonkey).toMap

object Solving:
  import DataDefs.*, Op.*, Tree.*

  def eval(monkey: Monkey)(using monkeys: Monkeys, yells: Yells): Long =
    yells.getOrElseUpdate(
      monkey,
      monkeys(monkey) match
        case Leaf(value) => value
        case Branch(left, right, op) =>
          op match
            case Add => eval(left) + eval(right)
            case Sub => eval(left) - eval(right)
            case Mul => eval(left) * eval(right)
            case Div => eval(left) / eval(right)
    )

  def solve1(lines: Seq[String]) =
    given Yells   = MMap[Monkey, Long]()
    given Monkeys = Parsing.parse(lines)
    eval("root")

  def solve2(lines: Seq[String])(value: Long) =
    given Yells            = MMap[Monkey, Long]("humn" -> value)
    given monkeys: Monkeys = Parsing.parse(lines)
    val root               = monkeys("root").asInstanceOf[Branch]
    val (left, right)      = (root.left, root.right)
    (eval(left), eval(right))

object Test:
  val file  = os.pwd / "2022" / "21" / "21.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)(301L)
// Test.res1 // part 1: 152
// Test.res2 // part 2: 301

object Main:
  val file  = os.pwd / "2022" / "21" / "21.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)(3_560_324_848_168L) // guess here!
// Main.res1 // part 1: 85616733059734
// Main.res2 // part 2: 3560324848168
