/*
--- Day 17: Chronospatial Computer ---
The Historians push the button on their strange device,
but this time, you all just feel like you're falling.

"Situation critical", the device announces in a familiar voice.
"Bootstrapping process failed. Initializing debugger...."

The small handheld device suddenly unfolds into an entire computer!
The Historians look around nervously before one of them tosses it to you.

This seems to be a 3-bit computer: its program is a list of 3-bit numbers
(0 through 7), like 0,1,2,3. The computer also has three registers named
A, B, and C, but these registers aren't limited to 3 bits and can instead
hold any integer.

The computer knows eight instructions, each identified by a 3-bit number
(called the instruction's opcode). Each instruction also reads the 3-bit
number after it as an input; this is called its operand.

A number called the instruction pointer identifies the position in the program
from which the next opcode will be read; it starts at 0, pointing at the first
3-bit number in the program. Except for jump instructions, the instruction pointer
increases by 2 after each instruction is processed (to move past the instruction's
opcode and its operand). If the computer tries to read an opcode past the end of
the program, it instead halts.

So, the program 0,1,2,3 would run the instruction whose opcode is 0 and pass it
the operand 1, then run the instruction having opcode 2 and pass it the operand 3,
then halt.

There are two types of operands; each instruction specifies the type of its operand.
The value of a literal operand is the operand itself.
For example, the value of the literal operand 7 is the number 7.
The value of a combo operand can be found as follows:
  Combo operands 0 through 3 represent literal values 0 through 3.
  Combo operand 4 represents the value of register A.
  Combo operand 5 represents the value of register B.
  Combo operand 6 represents the value of register C.
  Combo operand 7 is reserved and will not appear in valid programs.

The eight instructions are as follows:

The adv instruction (opcode 0) performs division.
The numerator is the value in the A register.
The denominator is found by raising 2 to the power of the instruction's combo operand.
(So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
The res of the division operation is truncated to an integer and
then written to the A register.

The bxl instruction (opcode 1) calculates the bitwise XOR of register B
and the instruction's literal operand, then stores the res in register B.

The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
(thereby keeping only its lowest 3 bits), then writes that value to the B register.

The jnz instruction (opcode 3) does nothing if the A register is 0.
However, if the A register is not zero, it jumps by setting the
instruction pointer to the value of its literal operand;
if this instruction jumps, the instruction pointer is not increased
by 2 after this instruction.

The bxc instruction (opcode 4) calculates the bitwise XOR of register B
and register C, then stores the res in register B.
(For legacy reasons, this instruction reads an operand but ignores it.)

The out instruction (opcode 5) calculates the value of its combo operand modulo 8,
then outputs that value. (If a program outputs multiple values,
they are separated by commas.)

The bdv instruction (opcode 6) works exactly like the adv instruction except
that the res is stored in the B register. (The numerator is still read
from the A register.)

The cdv instruction (opcode 7) works exactly like the adv instruction except
that the res is stored in the C register. (The numerator is still read
from the A register.)

Here are some examples of instruction operation:
  If register C contains 9, the program 2,6 would set register B to 1.
  If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
  If register A contains 2024, the program 0,1,5,4,3,0 would output
    4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
  If register B contains 29, the program 1,7 would set register B to 26.
  If register B contains 2024 and register C contains 43690,
    the program 4,0 would set register B to 44354.

The Historians' strange device has finished initializing its debugger and
is displaying some information about the program it is trying to run
(your puzzle input). For example:

Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0

Your first task is to determine what the program is trying to output.
To do this, initialize the registers to the given values,
then run the given program, collecting any output produced by out instructions.
(Always join the values produced by out instructions with commas.)
After the above program halts, its final output will be 4,6,3,5,6,3,5,2,1,0.

Using the information provided by the debugger,
initialize the registers to the given values, then run the program.
Once it halts, what do you get if you use commas to join the values
it output into a single string?

--- Part Two ---
Digging deeper in the device's manual, you discover the problem:
this program is supposed to output another copy of the program!
Unfortunately, the value in register A seems to have been corrupted.
You'll need to find a new value to which you can initialize register A
so that the program's output instructions produce an exact copy of the program itself.

For example:

Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0

This program outputs a copy of itself if register A is instead initialized to 117440.
(The original initial value of register A, 2024, is ignored.)

What is the lowest positive initial value for register A
that causes the program to output a copy of itself?
 */
object DataDefs:
  enum Inst:
    //    0    1    2    3    4    5    6    7
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv
  import Inst.*
  extension (opcode: Int) def toInst = Inst.values(opcode)

  type Ptr  = Int
  type Reg  = Long
  type Regs = (a: Reg, b: Reg, c: Reg)
  type Prog = List[Int]
  type Outp = List[Reg]
  type Comp = (regs: Regs, ptr: Ptr, outp: Outp)

  extension (c: Comp)
    def combo(operand: Int) = operand match
      case 0 | 1 | 2 | 3 => operand.toLong
      case 4             => c.regs.a
      case 5             => c.regs.b
      case 6             => c.regs.c
      case 7             => -1L // reserved

    def halted(using prog: Prog): Boolean  = c.ptr < 0 || prog.size <= c.ptr
    def outSelf(using prog: Prog): Boolean = c.outp.reverse.zip(prog).forall(_ == _)

    def execInst(using prog: Prog): Comp = // assume not halted
      val (opcode, literal) = (prog(c.ptr), prog(c.ptr + 1))
      val (inst, combo)     = (opcode.toInst, c.combo(literal))
      inst match
        case Adv => ((c.regs.a / (1L << combo), c.regs.b, c.regs.c), c.ptr + 2, c.outp)
        case Bxl => ((c.regs.a, c.regs.b ^ literal.toLong, c.regs.c), c.ptr + 2, c.outp)
        case Bst => ((c.regs.a, combo % 8, c.regs.c), c.ptr + 2, c.outp)
        case Jnz => (c.regs, if c.regs.a == 0L then c.ptr + 2 else literal, c.outp)
        case Bxc => ((c.regs.a, c.regs.b ^ c.regs.c, c.regs.c), c.ptr + 2, c.outp)
        case Out => (c.regs, c.ptr + 2, (combo % 8L) :: c.outp)
        case Bdv => ((c.regs.a, c.regs.a / (1L << combo), c.regs.c), c.ptr + 2, c.outp)
        case Cdv => ((c.regs.a, c.regs.b, c.regs.a / (1L << combo)), c.ptr + 2, c.outp)

object Solving:
  import DataDefs.*

  def solve1(regs: Regs)(using Prog) =
    var comp: Comp = (regs = regs, ptr = 0, outp = Nil)
    while !comp.halted do comp = comp.execInst
    comp.outp.reverse.mkString(",")

  def solve2(regs: Regs)(using prog: Prog) =
    import util.boundary, boundary.break

    var a = 0L
    boundary:
      while true do
        var comp: Comp = (regs = (a = a, b = 0L, c = 0L), ptr = 0, outp = Nil)
        while !comp.halted && comp.outSelf do
          comp = comp.execInst
          if comp.outp.reverse == prog then break()
        a += 1L
    a

object Test1:
  import DataDefs.*
  val regs: Regs  = (a = 729L, b = 0L, c = 0L)
  given Prog      = List(0, 1, 5, 4, 3, 0)
  lazy val res = Solving.solve1(regs)
// Test1.res // part 1: 4,6,3,5,6,3,5,2,1,0

object Test2:
  import DataDefs.*
  val regs: Regs  = (a = 2024L, b = 0L, c = 0L)
  given Prog      = List(0, 3, 5, 4, 3, 0)
  lazy val res = Solving.solve2(regs)
// Test2.res // part 2: 117440

object Main:
  import DataDefs.*
  val regs: Regs   = (a = 63281501L, b = 0L, c = 0L)
  given prog: Prog = List(2, 4, 1, 5, 7, 5, 4, 5, 0, 3, 1, 6, 5, 5, 3, 0)
  lazy val res1 = Solving.solve1(regs)
  lazy val res2 = Solving.solve2(regs)
// Main.res1 // part 1: 3,4,3,1,7,6,5,6,0
// Main.res2 // part 2:
