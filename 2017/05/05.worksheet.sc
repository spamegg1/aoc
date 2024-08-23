/*
--- Day 5: A Maze of Twisty Trampolines, All Alike ---
An urgent interrupt arrives from the CPU: it's trapped in a maze of jump instructions,
and it would like assistance from any programs with spare cycles to help find the exit.

The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to
the previous instruction, and 2 skips the next one. Start at the first instruction in the
list. The goal is to follow the jumps until one leads outside the list.

In addition, these instructions are a little strange; after each jump, the offset of that
instruction increases by 1. So, if you come across an offset of 3, you would move three
instructions forward, but change it to a 4 for the next time it is encountered.

For example, consider the following list of jump offsets:
0
3
0
1
-3

Positive jumps ("forward") move downward; negative jumps move upward. For legibility in
this example, these offset values will be written all on one line, with the current
instruction marked in parentheses.
The following steps would be taken before an exit is found:
  (0) 3  0  1  -3  - before we have taken any steps.
  (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all).
    Fortunately, the instruction is then incremented to 1.
  2 (3) 0  1  -3  - step forward because of the instruction we just modified.
    The first instruction is incremented again, now to 2.
  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.

In this example, the exit is reached in 5 steps.
How many steps does it take to reach the exit?

--- Part Two ---
Now, the jumps are even stranger: after each jump, if the offset was three or more,
instead decrease it by 1. Otherwise, increase it by 1 as before.

Using this rule with the above example, the process now takes 10 steps, and the offset
values after finding the exit are left as 2 3 2 3 -1.

How many steps does it now take to reach the exit?
 */
object DataDefs:
  case class State(instrs: Seq[Int], index: Int, counter: Int, escaped: Boolean):
    def nextState(rule: Int => Int): State =
      val jump = instrs(index)
      val newIndex = index + jump
      State(
        instrs.updated(index, rule(jump)),
        newIndex,
        counter + 1,
        newIndex < 0 || instrs.size <= newIndex
      )

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = State(lines.map(_.toInt), 0, 0, false)

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String])(rule: Int => Int): Int =
    @annotation.tailrec
    def helper(state: State): State =
      if state.escaped then state else helper(state.nextState(rule))
    helper(Parsing.parse(lines)).counter

  def solve1(lines: Seq[String]): Int = solve(lines)(_ + 1)
  def solve2(lines: Seq[String]): Int =
    val rule = (jump: Int) => if 3 <= jump then jump - 1 else jump + 1
    solve(lines)(rule)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "05.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 5
Testing.result2 // part 2: 10

object Main:
  private lazy val lines = os.read.lines(os.pwd / "05.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 342669
// Main.result2 // part 2: 25136209
