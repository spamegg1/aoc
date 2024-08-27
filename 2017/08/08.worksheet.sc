/*
--- Day 8: I Heard You Like Registers ---
You receive a signal directly from the CPU.
Because of your recent assistance with jump instructions,
it would like you to compute the result of a series of unusual register instructions.

Each instruction consists of several parts: the register to modify,
whether to increase or decrease that register's value,
the amount by which to increase or decrease it, and a condition.
If the condition fails, skip the instruction without modifying the register.
The registers all start at 0. The instructions look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10

These instructions would be processed as follows:
  Because a starts at 0, it is not greater than 1, and so b is not modified.
  a is increased by 1 (to 1) because b is less than 5 (it is 0).
  c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
  c is increased by -20 (to -10) because c is equal to 10.

After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you
what all the registers are named, and leaves that to you to determine.

What is the largest value in any register after completing
the instructions in your puzzle input?

--- Part Two ---
To be safe, the CPU also needs to know the highest value held
in any register during this process so that it can decide how much
memory to allocate to these operations. For example, in the above
instructions, the highest value ever held was 10
(in register c after the third instruction was evaluated).

 */
object DataDefs:
  type Register = String
  type Registers = Map[Register, Int]
  type Comparison = (Int, Int) => Boolean

  case class Condition(reg: Register, value: Int, comp: Comparison):
    def holds(regs: Registers) = comp(regs(reg), value)

  case class Instr(reg: Register, value: Int, cond: Condition):
    def process(regs: Registers): Registers =
      regs.updated(reg, regs(reg) + (if cond.holds(regs) then value else 0))

  object Registers:
    def apply(instrs: Seq[Instr]): Registers =
      val allRegisters = instrs.flatMap(instr => Seq(instr.reg, instr.cond.reg)).distinct
      allRegisters.map(reg => reg -> 0).toMap

  extension (s: String)
    def toComp: Comparison = s match
      case ">"  => _ > _
      case ">=" => _ >= _
      case "<"  => _ < _
      case "<=" => _ <= _
      case "==" => _ == _
      case "!=" => _ != _

    def incDec(value: Int): Int = s match
      case "inc" => value
      case "dec" => -value

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): Instr = line match
    case s"$writeTo $opr $write if $readFrom $comp $int" =>
      Instr(writeTo, opr.incDec(write.toInt), Condition(readFrom, int.toInt, comp.toComp))

  def parse(lines: Seq[String]): Seq[Instr] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    val registers = Registers(instrs)
    instrs.foldLeft(registers)((regs, instr) => instr.process(regs)).values.max

  def solve2(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    val registers = Registers(instrs)
    var maxSoFar = 0
    val result = instrs.foldLeft(registers)((regs, instr) =>
      maxSoFar = math.max(maxSoFar, regs.values.max)
      instr.process(regs)
    )
    maxSoFar

object Testing:
  lazy val lines = os.read.lines(os.pwd / "2017" / "08" / "08.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 1
// Testing.result2 // part 2: 10

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2017" / "08" / "08.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 5946
// Main.result2 // part 2: 6026
