/*
--- Day 23: Coprocessor Conflagration ---
You decide to head directly to the CPU and fix the printer from there.
As you get close, you find an experimental coprocessor doing so much work
that the local programs are afraid it will halt and catch fire.
This would cause serious issues for the rest of the computer,
so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on that tablet.
The general functionality seems very similar, but some of the instructions are different:
  set X Y sets register X to the value of Y.
  sub X Y decreases register X by the value of Y.
  mul X Y sets register X to the result of multiplying
    the value contained in register X by the value of Y.
  jnz X Y jumps with an offset of the value of Y,
    but only if the value of X is not zero.
    (An offset of 2 skips the next instruction,
    an offset of -1 jumps to the previous instruction, and so on.)

Only the instructions listed above are used.
The eight registers here, named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode,
which allows for testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul instruction invoked?

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "23.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1:
// Testing.result2 // part 2:

object Main:
  private lazy val lines = os.read.lines(os.pwd / "23.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1:
// Main.result2 // part 2:
