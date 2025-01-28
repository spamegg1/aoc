/*
--- Day 23: Safe Cracking ---
This is one of the top floors of the nicest tower in EBHQ.
The Easter Bunny's private office is here, complete with a safe hidden behind a painting,
and who wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry.
A sticky note attached to the safe has a password hint on it: "eggs".
The painting is of a large rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display;
instead, the keypad comes apart in your hands, apparently having been smashed.
Behind it is some kind of socket - one that matches a connector in your
prototype computer! You pull apart the smashed keypad and extract the logic
circuit, plug it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the safe.
You extract the assembunny code from the logic chip (your puzzle input).

The code looks like it uses almost the same architecture and instruction set
that the monorail computer used! You should be able to use the same assembunny
interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does:
positive means forward; negative means backward):
  For one-argument instructions, inc becomes dec, and all other one-argument
    instructions become inc.
  For two-argument instructions, jnz becomes cpy,
    and all other two-instructions become jnz.
  The arguments of a toggled instruction are not affected.
  If an attempt is made to toggle an instruction outside the program, nothing happens.
  If toggling produces an invalid instruction (like cpy 1 2)
    and an attempt is later made to execute that instruction, skip it instead.
  If tgl toggles itself (for example, if a is 0, tgl a would target itself and
    become inc a), the resing instruction is not executed
    until the next time it is reached.

For example, given this program:
cpy 2 a
tgl a
tgl a
tgl a   -> inc a
cpy 1 a -> jnz 1 a
dec a
dec a
  cpy 2 a initializes register a to 2.
  The first tgl a toggles an instruction a (2) away from it,
    which changes the third tgl a into inc a.
  The second tgl a also modifies an instruction 2 away from it,
    which changes the cpy 1 a into jnz 1 a.
  The fourth line, which is now inc a, increments a to 3.
  Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead,
    skipping the dec a instructions.
In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry
(the number of eggs, 7) in register a, run the code,
and then send the value left in register a to the safe.
What value should be sent to the safe?

--- Part Two ---
The safe doesn't open, but it does make several angry noises to express its frustration.
You're quite sure your logic is working correctly, so the only other thing is...
you check the painting again. As it turns out, colored eggs are still eggs.
Now you count 12.

As you run the program with this new input,
the prototype computer begins to overheat.
You wonder what's taking so long, and
whether the lack of any instruction more powerful than "add one"
has anything to do with it. Don't bunnies usually multiply?

Anyway, what value should actually be sent to the safe?
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
    case CopyValReg(value: Int, to: Reg)
    case CopyRegReg(from: Reg, to: Reg)
    case Inc(reg: Reg)
    case Dec(reg: Reg)
    case JnzValVal(notZero: Int, offset: Int)
    case JnzValReg(notZero: Int, reg: Reg)
    case JnzRegVal(notZero: Reg, offset: Int)
    case JnzRegReg(notZero: Reg, reg: Reg)
    case Toggle(reg: Reg)
    case Illegal

    def toggle = this match
      case CopyValReg(value, to)      => JnzValReg(value, to)
      case CopyRegReg(from, to)       => JnzRegReg(from, to)
      case Inc(reg)                   => Dec(reg)
      case Dec(reg)                   => Inc(reg)
      case JnzValReg(notZero, reg)    => CopyValReg(notZero, reg)
      case JnzRegReg(notZero, reg)    => CopyRegReg(notZero, reg)
      case JnzValVal(notZero, offset) => Illegal // these cause illegal copy instructions
      case JnzRegVal(notZero, offset) => Illegal
      case Toggle(reg)                => Inc(reg)
      case Illegal                    => Illegal
  import Instr.*

  case class State(regs: Map[Reg, Int], instrs: Seq[Instr], ptr: Int):
    def halted = ptr < 0 || instrs.size <= ptr
    def handleInstr(instr: Instr): State = instr match
      case CopyValReg(value, to) => copy(regs = regs.updated(to, value), ptr = ptr + 1)
      case CopyRegReg(from, to) =>
        copy(regs = regs.updated(to, regs(from)), ptr = ptr + 1)
      case Inc(reg) => copy(regs = regs.updated(reg, regs(reg) + 1), ptr = ptr + 1)
      case Dec(reg) => copy(regs = regs.updated(reg, regs(reg) - 1), ptr = ptr + 1)
      case JnzValVal(notZero, offset) =>
        copy(ptr = ptr + (if notZero != 0 then offset else 1))
      case JnzValReg(notZero, reg) =>
        copy(ptr = ptr + (if notZero != 0 then regs(reg) else 1))
      case JnzRegVal(notZero, offset) =>
        copy(ptr = ptr + (if regs(notZero) != 0 then offset else 1))
      case JnzRegReg(notZero, reg) =>
        copy(ptr = ptr + (if regs(notZero) != 0 then regs(reg) else 1))
      case Toggle(reg) =>
        val newPtr = ptr + regs(reg)
        if newPtr < 0 || instrs.size <= newPtr then copy(ptr = ptr + 1)
        else
          val newInstrs = instrs.updated(ptr + regs(reg), instrs(ptr + regs(reg)).toggle)
          copy(instrs = newInstrs, ptr = ptr + 1)
      case Illegal => copy(ptr = ptr + 1)

    def handleCurrentInst = handleInstr(instrs(ptr))

  object State:
    def apply(instrs: Seq[Instr]) = // change this for parts 1/2
      new State(Map(A -> 12, B -> 0, C -> 0, D -> 0), instrs, 0)

object Parsing:
  import DataDefs.*, Instr.*

  def parseLine(line: String): Instr = line match
    case s"inc $reg" => Inc(reg.toReg)
    case s"dec $reg" => Dec(reg.toReg)
    case s"tgl $reg" => Toggle(reg.toReg)
    case s"cpy $regOrVal $reg" =>
      regOrVal.toIntOption match
        case None        => CopyRegReg(regOrVal.toReg, reg.toReg)
        case Some(value) => CopyValReg(value, reg.toReg)
    case s"jnz $regOrVal1 $regOrVal2" =>
      (regOrVal1.toIntOption, regOrVal2.toIntOption) match
        case (None, None)         => JnzRegReg(regOrVal1.toReg, regOrVal2.toReg)
        case (None, Some(value))  => JnzRegVal(regOrVal1.toReg, value)
        case (Some(value), None)  => JnzValReg(value, regOrVal2.toReg)
        case (Some(v1), Some(v2)) => JnzValVal(v1, v2)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) =
    var state = State(Parsing.parse(lines))
    while !state.halted do state = state.handleCurrentInst
    state.regs(Reg.A)

object Test:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "23" / "23.test.input.txt")
  lazy val res = Solving.solve(lines)
// Test.res // part 1: 3

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "23" / "23.input.txt")
  lazy val res = Solving.solve(lines)
// Main.res // part 1: 10584, part 2: 479007144
