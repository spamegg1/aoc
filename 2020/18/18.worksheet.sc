/*
--- Day 18: Operation Order ---
As you look out the window and notice a heavily-forested continent slowly
appear over the horizon, you are interrupted by the child sitting next to you.
They're curious if you could help them with their math homework.

Unfortunately, it seems like this "math" follows different rules than you remember.

The homework (your puzzle input) consists of a series of expressions that consist
of addition (+), multiplication (*), and parentheses ((...)).
Just like normal math, parentheses indicate that the expression
inside must be evaluated before it can be used by the surrounding expression.
Addition still finds the sum of the numbers on both sides of the operator,
and multiplication still finds the product.

However, the rules of operator precedence have changed. Rather than evaluating
multiplication before addition, the operators have the same precedence,
and are evaluated left-to-right regardless of the order in which they appear.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
      9   + 4 * 5 + 6
         13   * 5 + 6
             65   + 6
                 71

Parentheses can override this order; for example, here is what happens if
parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):

1 + (2 * 3) + (4 * (5 + 6))
1 +    6    + (4 * (5 + 6))
     7      + (4 * (5 + 6))
     7      + (4 *   11   )
     7      +     44
            51

Here are a few more examples:
  2 * 3 + (4 * 5) becomes 26.
  5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
  5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
  ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.

Before you can help with the homework, you need to understand it yourself.
Evaluate the expression on each line of the homework;
what is the sum of the resing values?

--- Part Two ---
You manage to answer the child's questions and
they finish part 1 of their homework,
but get stuck when they reach the next section: advanced math.

Now, addition and multiplication have different precedence levels,
but they're not the ones you're familiar with.
Instead, addition is evaluated before multiplication.

For example, the steps to evaluate the expression
1 + 2 * 3 + 4 * 5 + 6 are now as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
  3   *   7   * 5 + 6
  3   *   7   *  11
     21       *  11
         231

Here are the other examples from above:
  1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
  2 * 3 + (4 * 5) becomes 46.
  5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
  5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
  ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.

What do you get if you add up the ress of evaluating
the homework problems using these new rules?
 */
object DataDefs:
  enum Token:
    case Add
    case Mul
    case Num(n: Long)
    case Par(expr: Tokens)

  type Tokens = List[Token]

object Parsing:
  import DataDefs.*, Token.*

  @annotation.tailrec
  def takeBalanced(line: String, acc: String, left: Int, right: Int): (String, String) =
    if left == right then (acc, line)
    else
      takeBalanced(
        line.tail,
        acc.appended(line.head),
        left + (if line.head == '(' then 1 else 0),
        right + (if line.head == ')' then 1 else 0)
      )

  def parseLine(acc: List[Token])(line: String): Tokens = line.headOption match
    case None => acc.reverse // we will add tokens in reverse order with cons
    case Some(value) =>
      value match
        case ' ' => parseLine(acc)(line.tail)
        case '(' =>
          val (parens, rest) = takeBalanced(line.tail, "", 1, 0)
          parseLine(Par(parseLine(Nil)(parens.init)) :: acc)(rest)
        case '+' => parseLine(Add :: acc)(line.tail)
        case '*' => parseLine(Mul :: acc)(line.tail)
        case n   => parseLine(Num(n.asDigit.toLong) :: acc)(line.tail)

  def parse(lines: Seq[String]) = lines map parseLine(Nil)

object Solving:
  import DataDefs.*, Token.*

  // mutual recursion, this works for both parts
  def one(many: Tokens => Long)(token: Token): Long = token match // parts 1, 2
    case Num(n)    => n
    case Par(expr) => many(expr)
    case _         => 0L

  // mutual recursion, part 1
  def many1(acc: Long)(tokens: Tokens): Long = tokens match
    case Nil => acc
    case t1 :: Add :: t2 :: rest =>
      many1(one(many1(0L))(t1) + one(many1(0L))(t2) + acc)(rest)
    case t1 :: Mul :: t2 :: rest =>
      many1(one(many1(0L))(t1) * one(many1(0L))(t2) + acc)(rest)
    case Add :: token :: rest => many1(acc + one(many1(0L))(token))(rest)
    case Mul :: token :: rest => many1(acc * one(many1(0L))(token))(rest)
    case _                    => acc

  // mutual recursion, part 2
  def many2(acc: Long)(unused: Tokens)(tokens: Tokens): Long = tokens match
    case Nil                             => many2(acc)(Nil)(unused)
    case Num(n) :: Add :: Num(m) :: rest => many2(acc + n + m)(unused)(rest)
    case Add :: Num(n) :: rest           => many2(acc + n)(unused)(rest)
    case _                               => acc

  private def solve(eval: Tokens => Long)(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(eval)
    .sum

  val solve1 = solve(many1(0L))
  val solve2 = solve(many2(0L)(Nil))

object Test:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "18" / "18.test.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Test.res1 // part 1: 71,51,26,437,12240,13632 = 26457
// Test.res2 // part 2: 231,51,46,1445,669060,23340 = 694173

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "18" / "18.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 45840336521334
// Main.res2 // part 2:
