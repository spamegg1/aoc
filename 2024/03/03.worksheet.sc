/*
--- Day 3: Mull It Over ---
"Our computers are having issues, so I have no idea if we have any
Chief Historians in stock! You're welcome to check the warehouse, though,"
says the mildly flustered shopkeeper at the North Pole Toboggan Rental Shop.
The Historians head out to take a look.

The shopkeeper turns to you. "Any chance you can see
why our computers are having issues again?"

The computer appears to be trying to run a program,
but its memory (your puzzle input) is corrupted.
All of the instructions have been jumbled up!

It seems like the goal of the program is just to multiply some numbers.
It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers.
For instance, mul(44,46) multiplies 44 by 46 to get a res of 2024.
Similarly, mul(123,4) would multiply 123 by 4.

However, because the program's memory has been corrupted,
there are also many invalid characters that should be ignored,
even if they look like part of a mul instruction.
Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.

For example, consider the following section of corrupted memory:

xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

Only the four highlighted sections are real mul instructions.
Adding up the res of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).

Scan the corrupted memory for uncorrupted mul instructions.
What do you get if you add up all of the ress of the multiplications?

--- Part Two ---
As you scan through the corrupted memory, you notice that some of the
conditional statements are also still intact.
If you handle some of the uncorrupted conditional statements in the program,
you might be able to get an even more accurate res.

There are two new instructions you'll need to handle:
  The do() instruction enables future mul instructions.
  The don't() instruction disables future mul instructions.

Only the most recent do() or don't() instruction applies.
At the beginning of the program, mul instructions are enabled.

For example:
  xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

This corrupted memory is similar to the example from before,
but this time the mul(5,5) and mul(11,8) instructions are disabled
because there is a don't() instruction before them.
The other mul instructions function normally,
including the one at the end that gets re-enabled by a do() instruction.

This time, the sum of the ress is 48 (2*4 + 8*5).

Handle the new instructions; what do you get if you add up
all of the ress of just the enabled multiplications?
 */
object Parsing:
  val regex                     = """(don't|do|mul\([0-9]+,[0-9]+\))""".r
  def parseLine(line: String)   = regex.findAllMatchIn(line).toSeq.map(_.toString)
  def parse(lines: Seq[String]) = lines.flatMap(parseLine)

object Solving:
  def multiply(instr: String): Long = instr match
    case s"mul($m,$n)" => m.toLong * n.toLong
    case _             => 0L

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .map(multiply)
    .sum

  def solve2(lines: Seq[String]) =
    val instrs  = Parsing.parse(lines)
    var enabled = true
    var res  = 0L
    for instr <- instrs do
      if instr == "don't" then enabled = false
      else if instr == "do" then enabled = true
      else if enabled then res += multiply(instr)
      else ()
    res

object Test:
  lazy val lines1  = os.read.lines(os.pwd / "2024" / "03" / "03.test.input.txt")
  lazy val lines2  = os.read.lines(os.pwd / "2024" / "03" / "03.test.input.2.txt")
  lazy val res1 = Solving.solve1(lines1)
  lazy val res2 = Solving.solve2(lines2)
// Test.res1 // part 1: 161
// Test.res2 // part 2: 48

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2024" / "03" / "03.input.txt")
  lazy val res1       = Solving.solve1(lines)
  lazy val res2       = Solving.solve2(lines)
// Main.res1 // part 1: 173529487
// Main.res2 // part 2: 99532691
