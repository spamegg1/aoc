/*
--- Day 21: Monkey Math ---
The monkeys are back! You're worried they're going to try to steal your stuff again, but
it seems like they're just holding their ground and making various monkey noises at you.

Eventually, one of the elephants realizes you don't speak monkey and comes over to
interpret. As it turns out, they overheard you talking about trying to find the grove;
they can show you a shortcut if you answer their riddle.

Each monkey is given a job: either to yell a specific number or to yell the result of a
math operation. All of the number-yelling monkeys know their number from the start;
however, the math operation monkeys need to wait for two other monkeys to yell a number,
and those two other monkeys might also be waiting on other monkeys.

Your job is to work out the number the monkey named root will yell before the monkeys
figure it out themselves.

For example:

root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32

Each line contains the name of a monkey, a colon, and then the job of that monkey:
  A lone number means the monkey's job is simply to yell that number.
  A job like aaaa + bbbb means the monkey waits for monkeys aaaa and bbbb
    to yell each of their numbers; the monkey then yells the sum of those two numbers.
  aaaa - bbbb means the monkey yells aaaa's number minus bbbb's number.
  Job aaaa * bbbb will yell aaaa's number multiplied by bbbb's number.
  Job aaaa / bbbb will yell aaaa's number divided by bbbb's number.

So, in the above example, monkey drzm has to wait
for monkeys hmdt and zczc to yell their numbers.
Fortunately, both hmdt and zczc have jobs that involve simply yelling
a single number, so they do this immediately: 32 and 2.
Monkey drzm can then yell its number by finding 32 minus 2: 30.

Then, monkey sjmn has one of its numbers (30, from monkey drzm),
and already has its other number, 5, from dbpl.
This allows it to yell its own number by finding 30 multiplied by 5: 150.
This process continues until root yells a number: 152.

However, your actual situation involves considerably more monkeys.
What number will the monkey named root yell?

--- Part Two ---
Due to some kind of monkey-elephant-human mistranslation,
you seem to have misunderstood a few key details about the riddle.

First, you got the wrong job for the monkey named root;
specifically, you got the wrong math operation.
The correct operation for monkey root should be =,
which means that it still listens for two numbers
(from the same two monkeys as before),
but now checks that the two numbers match.

Second, you got the wrong monkey for the job starting with humn:.
It isn't a monkey - it's you.
Actually, you got the job wrong, too:
you need to figure out what number you need to yell
so that root's equality check passes.
(The number that appears after humn: in your input is now irrelevant.)

In the above example, the number you need to yell to pass root's equality test is 301.
(This causes root to get the same number, 150, from both of its monkeys.)

What number do you yell to pass root's equality test?
 */
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
  type Yell = Long
  type Yells = MMap[Monkey, Yell]

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
    given Yells = MMap[Monkey, Long]()
    given Monkeys = Parsing.parse(lines)
    eval("root")

  def solve2(lines: Seq[String])(value: Long) =
    given Yells = MMap[Monkey, Long]("humn" -> value)
    given monkeys: Monkeys = Parsing.parse(lines)
    val root = monkeys("root").asInstanceOf[Branch]
    val (left, right) = (root.left, root.right)
    (eval(left), eval(right))

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "21.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)(301L)
Testing.result1 // part 1: 152
Testing.result2 // part 2: 301

object Main:
  private lazy val lines = os.read.lines(os.pwd / "21.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)(3_560_324_848_168L) // guess here!
Main.result1 // part 1: 85616733059734
Main.result2 // part 2: 3560324848168
