package aoc2020.day18

object DataDefs:
  enum Parser(pat: String):
    val regex = pat.r
    def unapply(line: String): Option[(String, String)] = regex
      .findPrefixOf(line)
      .map(prefix => line.splitAt(prefix.length))
    case White extends Parser("\\s+")
    case Nat   extends Parser("\\d+")
    case Ope   extends Parser("\\(")
    case Clo   extends Parser("\\)")
    case Sum   extends Parser("\\+")
    case Prod  extends Parser("\\*")
  import Parser.*

  enum Token:
    case NatVal(value: String)
    case Open
    case Close
    case Add
    case Mul
  import Token.*

object Parsing:
  import DataDefs.*, Parser.*, Token.*

  @annotation.tailrec
  def helper1(remain: String, tokens: List[Token]): List[Token] = remain match
    case ""                => tokens.reverse
    case White(head, tail) => helper1(tail, tokens)
    case Nat(head, tail)   => helper1(tail, NatVal(head) :: tokens)
    case Ope(head, tail)   => helper1(tail, Open :: tokens)
    case Clo(head, tail)   => helper1(tail, Close :: tokens)
    case Sum(head, tail)   => helper1(tail, Add :: tokens)
    case Prod(head, tail)  => helper1(tail, Mul :: tokens)

  def infix(line: String): List[Token] = helper1(line, Nil)

  @annotation.tailrec
  def helper2(
      tokens: List[Token],
      output: List[Token],
      stack: List[Token]
  )(using priority: Map[Token, Int]): List[Token] = tokens match
    case Nil                         => output.reverse ++ stack
    case ((natural: NatVal) :: tail) => helper2(tail, natural :: output, stack)
    case (Open :: tail)              => helper2(tail, output, Open :: stack)
    case (Close :: tail) =>
      if stack.head == Open then helper2(tail, output, stack.tail)
      else helper2(tokens, stack.head :: output, stack.tail)
    case (operator :: tail) =>
      if stack.isEmpty || priority(stack.head) < priority(operator) then
        helper2(tail, output, operator :: stack)
      else helper2(tokens, stack.head :: output, stack.tail)

  def postfix(priority: Map[Token, Int])(infix: List[Token]): List[Token] =
    helper2(infix, Nil, Nil)(using priority)

object Solving:
  import DataDefs.*, Token.*

  @annotation.tailrec
  def helper(tokens: List[Token], stack: List[Long]): Long = tokens match
    case Nil                     => stack.head
    case (NatVal(value) :: tail) => helper(tail, value.toLong :: stack)
    case (operator :: tail) =>
      (operator, stack) match
        case (Add, first :: second :: rest) => helper(tail, (first + second) :: rest)
        case (Mul, first :: second :: rest) => helper(tail, (first * second) :: rest)
        case _                              => throw MatchError("Unreachable")

  def eval(postfix: List[Token]): Long = helper(postfix, Nil)

  def solve(priority: Map[Token, Int])(lines: Seq[String]): Long = lines.view
    .map(Parsing.infix)
    .map(Parsing.postfix(priority))
    .map(eval)
    .sum

  val prio1  = Map(Open -> 0, Mul -> 1, Add -> 1)
  val prio2  = Map(Open -> 0, Mul -> 1, Add -> 2)
  val solve1 = solve(prio1)
  val solve2 = solve(prio2)

object Test:
  lazy val file  = os.pwd / "2020" / "18" / "18.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "18" / "18.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 71,51,26,437,12240,13632 = 26457
  println(Test.res2) // part 2: 231,51,46,1445,669060,23340 = 694173
  println(Main.res1) // part 1: 45840336521334
  println(Main.res2) // part 2: 328920644404583
