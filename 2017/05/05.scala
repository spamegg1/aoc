package aoc2017.day05

object DataDefs:
  case class State(
      instrs: Array[Int],
      var index: Int,
      var counter: Int,
      var escaped: Boolean
  ):
    def nextState(rule: Int => Int): Unit =
      val jump     = instrs(index)
      val newIndex = index + jump
      instrs(index) = rule(jump)
      index = newIndex
      counter += 1
      escaped = newIndex < 0 || instrs.size <= newIndex

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = State(lines.map(_.toInt).toArray, 0, 0, false)

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String])(rule: Int => Int): Int =
    val state = Parsing.parse(lines)
    while !state.escaped do state.nextState(rule)
    state.counter

  def solve1(lines: Seq[String]): Int = solve(lines)(_ + 1)
  def solve2(lines: Seq[String]): Int =
    val rule = (jump: Int) => if 3 <= jump then jump - 1 else jump + 1
    solve(lines)(rule)

object Test:
  lazy val file  = os.pwd / "2017" / "05" / "05.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2017" / "05" / "05.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 5
  println(Test.res2) // part 2: 10
  println(Main.res1) // part 1: 342669
  println(Main.res2) // part 2: 25136209 slow! 20s, much faster with mutable Array.
