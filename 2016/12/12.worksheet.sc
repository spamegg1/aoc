/*
--- Day 12: Leonardo's Monorail ---
You finally reach the top floor of this building: a garden with a slanted glass ceiling.
Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies,
you manage to decrypt some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building -
it's a collection of buildings in the nearby area.
They're all connected by a local monorail, and
there's another building not far from here!
Unfortunately, being night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover
that the boot sequence expects a password.
The password-checking logic (your puzzle input) is easy to extract,
but the code it uses is strange: it's assembunny code
designed for the new computer you just assembled.
You'll have to execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and d)
that start at 0 and can hold any integer.
However, it seems to make use of only a few instructions:
  cpy x y copies x (either an integer or the value of a register) into register y.
  inc x increases the value of register x by one.
  dec x decreases the value of register x by one.
  jnz x y jumps to an instruction y away (positive means forward;
    negative means backward), but only if x is not zero.

The jnz instruction moves relative to itself:
  an offset of -1 would continue at the previous instruction, while
  an offset of 2 would skip over the next instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would
set register a to 41,
increase its value by 2,
decrease its value by 1, and then
skip the last dec a (because a is not zero, so the jnz a 2 skips it),
leaving register a at 42.
When you move past the last instruction, the program halts.

After executing the assembunny code in your puzzle input,
what value is left in register a?

--- Part Two ---
As you head down the fire escape to the monorail, you notice it didn't start;
register c needs to be initialized to the position of the ignition key.
If you instead initialize register c to be 1, what value is now left in register a?
 */
object DataDefs:
  enum Reg:
    case A, B, C, D
  import Reg.*

  extension (s: String)
    def toReg = s match
      case "a" => A
      case "b" => B
      case "c" => C
      case "d" => D

  enum Instr:
    case CopyVal(value: Int, to: Reg)
    case CopyReg(from: Reg, to: Reg)
    case Inc(reg: Reg)
    case Dec(reg: Reg)
    case JnzVal(notZero: Int, offset: Int)
    case JnzReg(notZero: Reg, offset: Int)
  import Instr.*

  case class State(regs: Map[Reg, Int], instrs: Seq[Instr], ptr: Int):
    def halted = ptr < 0 || instrs.size <= ptr
    def handleInstr(instr: Instr) = instr match
      case CopyVal(value, to) => copy(regs = regs.updated(to, value), ptr = ptr + 1)
      case CopyReg(from, to)  => copy(regs = regs.updated(to, regs(from)), ptr = ptr + 1)
      case Inc(reg) => copy(regs = regs.updated(reg, regs(reg) + 1), ptr = ptr + 1)
      case Dec(reg) => copy(regs = regs.updated(reg, regs(reg) - 1), ptr = ptr + 1)
      case JnzVal(notZero, offset) =>
        copy(ptr = ptr + (if notZero != 0 then offset else 1))
      case JnzReg(notZero, offset) =>
        copy(ptr = ptr + (if regs(notZero) != 0 then offset else 1))

  object State:
    def apply(instrs: Seq[Instr]) =
      new State(Map(A -> 0, B -> 0, C -> 1, D -> 0), instrs, 0) // change this for part1/2

object Parsing:
  import DataDefs.*, Instr.*

  def parseLine(line: String): Instr = line match
    case s"cpy $regOrVal $reg" =>
      regOrVal.toIntOption match
        case None        => CopyReg(regOrVal.toReg, reg.toReg)
        case Some(value) => CopyVal(value, reg.toReg)
    case s"inc $reg" => Inc(reg.toReg)
    case s"dec $reg" => Dec(reg.toReg)
    case s"jnz $regOrVal $offset" =>
      regOrVal.toIntOption match
        case None        => JnzReg(regOrVal.toReg, offset.toInt)
        case Some(value) => JnzVal(value, offset.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    var state = State(instrs)
    while !state.halted do state = state.handleInstr(instrs(state.ptr))
    state.regs(Reg.A)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "12.test.input.txt")
  lazy val result = Solving.solve(lines)
// Testing.result // part 1: 42

object Main:
  private lazy val lines = os.read.lines(os.pwd / "12.input.txt")
  lazy val result = Solving.solve(lines)
// Main.result // part 1: 318117, part 2: 9227771
