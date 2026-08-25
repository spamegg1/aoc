object DataDefs:
  enum Round:
    case Push(offset: Int)
    case Pop(offset: Int)

  case class Constraint(index: Int, delta: Int):
    def min = (1 + delta).max(1)
    def max = (9 + delta).min(9)

object Parsing:
  import DataDefs.*, Round.*

  def helper(round: Seq[String], n: Int) = round(n).split(" ").last.toInt
  def parse(input: Seq[String]): List[Round] = input
    .grouped(18)
    .toList
    .map: round =>
      if helper(round, 4) == 1 then Push(helper(round, 15))
      else Pop(helper(round, 5))

object Solving:
  import DataDefs.*, Round.*

  @annotation.tailrec
  def helper(
      remaining: List[(Round, Int)],
      stack: List[(Int, Int)],
      constraints: List[Constraint]
  )(using rounds: List[Round]): List[Constraint] =
    remaining match
      case Nil => constraints.sortBy(_.index)
      case (Push(firstValue), firstIndex) :: rest =>
        helper(remaining.tail, (firstValue, firstIndex) :: stack, constraints)
      case (Pop(secondValue), secondIndex) :: rest =>
        val (firstValue, firstIndex) = stack.head
        val delta                    = firstValue + secondValue
        helper(
          remaining.tail,
          stack.tail,
          Constraint(firstIndex, -delta) ::
            Constraint(secondIndex, delta) ::
            constraints
        )
  end helper

  def constraints(rounds: List[Round]): List[Constraint] =
    helper(rounds.zipWithIndex, Nil, Nil)(using rounds)

  def solve1(lines: Seq[String]) = constraints(Parsing.parse(lines)).map(_.max).mkString
  def solve2(lines: Seq[String]) = constraints(Parsing.parse(lines)).map(_.min).mkString

object Main:
  val file  = os.pwd / "2021" / "24" / "24.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 29989297949519
// Main.res2 // part 2: 19518121316118
