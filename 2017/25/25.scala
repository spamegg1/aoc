package aoc2017.day25

object DataDefs:
  type State  = String
  type Branch = (State, Boolean)
  type Rule   = (write: Boolean, step: Int, next: State)
  type Turing = (tape: Set[Int], position: Int, state: State)

  extension (t: Turing)
    def step(rules: Map[Branch, Rule]) =
      val rule     = rules(t.state -> t.tape.contains(t.position))
      val nextTape = if rule.write then t.tape + t.position else t.tape - t.position
      (nextTape, t.position + rule.step, rule.next)

object Parsing:
  import DataDefs.*

  def parseRule(block: Seq[Array[String]]): Rule =
    val write = block(0)(4) == "1"
    val step  = if block(1)(6) == "right" then 1 else -1
    val next  = block(2)(4)
    (write, step, next)

  def parse(lines: Seq[String]): (State, Int, Map[Branch, Rule]) =
    val trimmed = lines.map(_.init.trim.split(" "))
    val start   = trimmed(0)(3)
    val total   = trimmed(1)(5).toInt
    val rules = trimmed
      .drop(2)
      .grouped(10)
      .map: group =>
        val state = group(1)(2)
        Seq(
          (state, false) -> parseRule(group.drop(3)),
          (state, true)  -> parseRule(group.drop(7))
        )
    (start, total, rules.flatten.toMap)

object Solving:
  import DataDefs.*
  def solve(lines: Seq[String]) =
    val (start, total, rules) = Parsing.parse(lines)
    val initial               = (tape = Set[Int](), position = 0, state = start)
    Iterator
      .iterate(initial)(_.step(rules))
      .drop(total)
      .next()
      .tape
      .size

object Test:
  lazy val file  = os.pwd / "2017" / "25" / "25.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

object Main:
  lazy val file  = os.pwd / "2017" / "25" / "25.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

@main
def run: Unit =
  println(Test.res) // part 1: 3
  println(Main.res) // part 1: 2526
