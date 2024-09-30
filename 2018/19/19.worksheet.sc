/*
--- Day 19: Go With The Flow ---
With the Elves well on their way constructing the North Pole base,
you turn your attention back to understanding
the inner workings of programming the device.

You can't help but notice that the device's opcodes don't contain
any flow control like jump instructions.
The device's manual goes on to explain:

"In programs where flow control is required,
the instruction pointer can be bound to a register
so that it can be manipulated directly.
This way, setr/seti can function as absolute jumps,
addr/addi can function as relative jumps,
and other opcodes can cause truly fascinating effects."

This mechanism is achieved through a declaration like #ip 1,
which would modify register 1 so that accesses to it
let the program indirectly access the instruction pointer itself.
To compensate for this kind of binding,
there are now six registers (numbered 0 through 5);
the five not bound to the instruction pointer behave as normal.
Otherwise, the same rules apply as the last time you worked with this device.

When the instruction pointer is bound to a register,
its value is written to that register just before each
instruction is executed, and the value of that register
is written back to the instruction pointer immediately
after each instruction finishes execution.
Afterward, move to the next instruction by adding one
to the instruction pointer, even if the value in the
instruction pointer was just updated by an instruction.
(Because of this, instructions must effectively set the
instruction pointer to the instruction before the one they want executed next.)

The instruction pointer is 0 during the first instruction,
1 during the second, and so on.
If the instruction pointer ever causes the device to attempt
to load an instruction outside the instructions defined in the program,
the program instead immediately halts. The instruction pointer starts at 0.

It turns out that this new information is already proving useful:
the CPU in the device is not very powerful, and a background
process is occupying most of its time. You dump the background
process' declarations and instructions to a file (your puzzle input),
making sure to use the names of the opcodes rather than the numbers.

For example, suppose you have the following program:

#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5

When executed, the following instructions are executed.
Each line contains the value of the instruction pointer
at the time the instruction started, the values of the
six registers before executing the instructions (in square brackets),
the instruction itself, and the values of the six registers
after executing the instruction (also in square brackets).

ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]
// List(0, 0, 0, 0, 0, 0)
// List(1, 5, 0, 0, 0, 0)
// List(2, 5, 6, 0, 0, 0)
// List(3, 5, 6, 0, 0, 0)
// List(4, 5, 6, 11, 0, 0)
// List(5, 5, 6, 11, 0, 0)
// List(6, 5, 6, 11, 8, 0)
// List(7, 5, 6, 11, 8, 9)

In detail, when running this program, the following events occur:
  The first line (#ip 0) indicates that the instruction pointer
    should be bound to register 0 in this program.
    This is not an instruction, and so the value of the instruction
    pointer does not change during the processing of this line.
  The instruction pointer contains 0, and so the first instruction is
    executed (seti 5 0 1). It updates register 0 to the current instruction
    pointer value (0), sets register 1 to 5, sets the instruction pointer
    to the value of register 0 (which has no effect, as the instruction
    did not modify register 0), and then adds one to the instruction pointer.
  The instruction pointer contains 1, and so the second instruction, seti 6 0 2,
    is executed. This is very similar to the instruction before it: 6 is stored
    in register 2, and the instruction pointer is left with the value 2.
  The instruction pointer is 2, which points at the instruction addi 0 1 0.
    This is like a relative jump: the value of the instruction pointer, 2,
    is loaded into register 0. Then, addi finds the result of adding the
    value in register 0 and the value 1, storing the result, 3, back in
    register 0. Register 0 is then copied back to the instruction pointer,
    which will cause it to end up 1 larger than it would have otherwise and
    skip the next instruction (addr 1 2 3) entirely. Finally, 1 is added
    to the instruction pointer.
  The instruction pointer is 4, so the instruction setr 1 0 0 is run.
    This is like an absolute jump: it copies the value contained in
    register 1, 5, into register 0, which causes it to end up in the
    instruction pointer. The instruction pointer is then incremented,
    	leaving it at 6.
  The instruction pointer is 6, so the instruction seti 9 0 5 stores 9
    into register 5. The instruction pointer is incremented,
    causing it to point outside the program, and so the program ends.

What value is left in register 0 when the background process halts?

--- Part Two ---
A new background process immediately spins up in its place.
It appears identical, but on closer inspection,
you notice that this time, register 0 started with the value 1.

What value is left in register 0 when this new background process halts?
 */
object DataDefs:
  type Reg = Long

  enum OpType:
    case Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr,
      Eqir, Eqri, Eqrr
  import OpType.*

  extension (s: String)
    def toOp = s match
      case "addr" => Addr
      case "addi" => Addi
      case "mulr" => Mulr
      case "muli" => Muli
      case "banr" => Banr
      case "bani" => Bani
      case "borr" => Borr
      case "bori" => Bori
      case "setr" => Setr
      case "seti" => Seti
      case "gtir" => Gtir
      case "gtri" => Gtri
      case "gtrr" => Gtrr
      case "eqir" => Eqir
      case "eqri" => Eqri
      case "eqrr" => Eqrr

  case class Op(op: OpType, a: Int, b: Int, c: Int):
    def execute(regs: Seq[Reg]): Seq[Reg] = op match
      case Addr => regs.updated(c, regs(a) + regs(b))
      case Addi => regs.updated(c, regs(a) + b)
      case Mulr => regs.updated(c, regs(a) * regs(b))
      case Muli => regs.updated(c, regs(a) * b)
      case Banr => regs.updated(c, regs(a) & regs(b))
      case Bani => regs.updated(c, regs(a) & b)
      case Borr => regs.updated(c, regs(a) | regs(b))
      case Bori => regs.updated(c, regs(a) | b)
      case Setr => regs.updated(c, regs(a))
      case Seti => regs.updated(c, a)
      case Gtir => regs.updated(c, if a > b then 1 else 0)
      case Gtri => regs.updated(c, if regs(a) > b then 1 else 0)
      case Gtrr => regs.updated(c, if regs(a) > regs(b) then 1 else 0)
      case Eqir => regs.updated(c, if a == regs(b) then 1 else 0)
      case Eqri => regs.updated(c, if regs(a) == b then 1 else 0)
      case Eqrr => regs.updated(c, if regs(a) == regs(b) then 1 else 0)

  case class State(ipReg: Int, ptr: Int, regs: Seq[Reg], ops: Seq[Op], halted: Boolean):
    def next: State =
      try
        val op = ops(ptr)
        val regs1 = regs.updated(ipReg, ptr.toLong)
        val regs2 = op.execute(regs1)
        val newPtr = regs2(ipReg) + 1
        val regs3 = regs2.updated(ipReg, newPtr)
        copy(regs = regs3, ptr = newPtr.toInt)
      catch case _: IndexOutOfBoundsException => copy(halted = true)

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line match
    case s"$op $a $b $c" => Op(op.toOp, a.toInt, b.toInt, c.toInt)

  def parse(lines: Seq[String]) =
    val ipReg = lines.head.last.asDigit
    val ops = lines.tail map parseLine
    // State(ipReg, 0, Seq(0, 0, 0, 0, 0, 0), ops, false) // part 1
    State(ipReg, 0, Seq(0, 11, 10, 1, 10551394, 0), ops, false) // part 2

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    var state = Parsing.parse(lines)
    while !state.halted do state = state.next
    state.regs(0)

  def solve2(lines: Seq[String]) =
    var state = Parsing.parse(lines)
    var i = 0
    while i < 100 do
      println(state.regs)
      state = state.next
      i += 1

object Testing: // #ip 0
  private lazy val lines = os.read.lines(os.pwd / "2018" / "19" / "19.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
// Testing.result1 // part 1: 7

object Main: // #ip 2
  private lazy val lines = os.read.lines(os.pwd / "2018" / "19" / "19.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 1728
// Main.result2 // part 2: 18200448

// 10551394 keeps repeating
// its prime factorization is 10551394 = 2 x 7 x 167 x 4513
// so
// 1 +
// 2 + 7 + 167 + 4513 +
// 2*7 + 2*167 + 2*4513 + 7*167 + 7*4513 + 167*4513 +
// 2*7*167 + 2*7*4513 + 2*167*4513 + 7*167*4513 +
// 2*7*167*4513
