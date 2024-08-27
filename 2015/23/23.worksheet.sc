/*
--- Day 23: Opening the Turing Lock ---
Little Jane Marie just got her very first computer for Christmas
from some unknown benefactor. It comes with instructions and an example program,
but the computer itself seems to be malfunctioning.
She's curious what the program does, and would like you to help her run it.

The manual explains that the computer supports two registers and six instructions
(truly, it goes on to remind the reader, a state-of-the-art technology).
The registers are named a and b, can hold any non-negative integer,
and begin with a value of 0. The instructions are as follows:
  hlf r sets register r to half its current value,
    then continues with the next instruction.
  tpl r sets register r to triple its current value,
    then continues with the next instruction.
  inc r increments register r, adding 1 to it,
    then continues with the next instruction.
  jmp offset is a jump; it continues with the instruction offset away relative to itself.
  jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
  jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).

All three jump instructions work with an offset relative to that instruction.
The offset is always written with a prefix + or - to indicate the direction of the jump
(forward or backward, respectively).
For example, jmp +1 would simply continue with the next instruction,
while jmp +0 would continuously jump back to itself forever.
The program exits when it tries to run an instruction beyond the ones defined.

For example, this program sets a to 2,
because the jio instruction causes it to skip the tpl instruction:

inc a
jio a, +2
tpl a
inc a

What is the value in register b when the program
in your puzzle input is finished executing?

 */
object DataDefs:
  type Offset = Int

  enum Register:
    case A, B
  import Register.*

  extension (s: String)
    def toRegister = s match
      case "a" => A
      case "b" => B

  enum Instr:
    case Hlf(reg: Register)
    case Tpl(reg: Register)
    case Inc(reg: Register)
    case Jmp(offset: Offset)
    case Jie(reg: Register, offset: Offset)
    case Jio(reg: Register, offset: Offset)
  import Instr.*

  case class State(ptr: Int, a: Int, b: Int)(using instrs: Seq[Instr]):
    lazy val halted = ptr < 0 || ptr >= instrs.size

    def updateRegister(reg: Register, fun: Int => Int): State = reg match
      case A => copy(ptr = ptr + 1, a = fun(a))
      case B => copy(ptr = ptr + 1, b = fun(b))

    def updatePointer(reg: Register, fun: Int => Boolean, offset: Int): State =
      val check = reg match
        case A => fun(a)
        case B => fun(b)
      copy(ptr = ptr + (if check then offset else 1))

    def nextState(instr: Instr): State = instr match
      case Hlf(reg)         => updateRegister(reg, State.half)
      case Tpl(reg)         => updateRegister(reg, State.triple)
      case Inc(reg)         => updateRegister(reg, State.incr)
      case Jmp(offset)      => copy(ptr = ptr + offset)
      case Jie(reg, offset) => updatePointer(reg, State.isEven, offset)
      case Jio(reg, offset) => updatePointer(reg, State.isOne, offset)

    def next = nextState(instrs(ptr))

  object State:
    val half = (x: Int) => x / 2
    val triple = (x: Int) => x * 3
    val incr = (x: Int) => x + 1
    val isEven = (x: Int) => x % 2 == 0
    val isOne = (x: Int) => x == 1

object Parsing:
  import DataDefs.*, Instr.*, Register.*

  def parseLine(line: String): Instr = line match
    case s"hlf $reg"          => Hlf(reg.toRegister)
    case s"tpl $reg"          => Tpl(reg.toRegister)
    case s"inc $reg"          => Inc(reg.toRegister)
    case s"jmp $offset"       => Jmp(offset.toInt)
    case s"jie $reg, $offset" => Jie(reg.toRegister, offset.toInt)
    case s"jio $reg, $offset" => Jio(reg.toRegister, offset.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    given Seq[Instr] = Parsing.parse(lines)
    var state = State(0, 0, 0)
    while !state.halted do state = state.next
    state

  def solve2(lines: Seq[String]) =
    given Seq[Instr] = Parsing.parse(lines)
    var state = State(0, 1, 0)
    while !state.halted do state = state.next
    state

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "23" / "23.test.input.txt")
  lazy val result = Solving.solve1(lines).a
// Testing.result // part 1: 2

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "23" / "23.input.txt")
  lazy val result1 = Solving.solve1(lines).b
  lazy val result2 = Solving.solve2(lines).b
// Main.result1 // part 1: 307
// Main.result2 // part 2: 160
