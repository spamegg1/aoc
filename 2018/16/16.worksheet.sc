/*
--- Day 16: Chronal Classification ---
As you see the Elves defend their hot chocolate successfully,
you go back to falling through time. This is going to become a problem.

If you're ever going to return to your own time,
you need to understand how this device on your wrist works.
You have a little while before you reach your next destination,
and with a bit of trial and error, you manage to pull up a
programming manual on the device's tiny screen.

According to the manual, the device has four registers
(numbered 0 through 3) that can be manipulated by instructions
containing one of 16 opcodes. The registers start with the value 0.

Every instruction consists of four values: an opcode, two inputs
(named A and B), and an output (named C), in that order.
The opcode specifies the behavior of the instruction and how
the inputs are interpreted. The output, C, is always treated as a register.

In the opcode descriptions below, if something says "value A",
it means to take the number given as A literally.
(This is also called an "immediate" value.)
If something says "register A", it means to use the number given as A
to read from (or write to) the register with that number.
So, if the opcode addi adds register A and value B,
storing the result in register C, and the instruction
addi 0 7 3 is encountered, it would add 7 to the value
contained by register 0 and store the sum in register 3,
never modifying registers 0, 1, or 2 in the process.

Many opcodes are similar except for how they interpret their arguments.
The opcodes fall into seven general categories:

Addition:
  addr (add register) stores into register C
    result of adding register A and register B.
  addi (add immediate) stores into register C
    the result of adding register A and value B.

Multiplication:
  mulr (multiply register) stores into register C
    the result of multiplying register A and register B.
  muli (multiply immediate) stores into register C
    the result of multiplying register A and value B.

Bitwise AND:
  banr (bitwise AND register) stores into register C the
    result of the bitwise AND of register A and register B.
  bani (bitwise AND immediate) stores into register C the
    result of the bitwise AND of register A and value B.

Bitwise OR:
  borr (bitwise OR register) stores into register C the
    result of the bitwise OR of register A and register B.
  bori (bitwise OR immediate) stores into register C the
    result of the bitwise OR of register A and value B.

Assignment:
  setr (set register) copies the contents of register A into register C.
    (Input B is ignored.)
  seti (set immediate) stores value A into register C. (Input B is ignored.)

Greater-than testing:
  gtir (greater-than immediate/register) sets register C
    to 1 if value A is greater than register B.
    Otherwise, register C is set to 0.
  gtri (greater-than register/immediate) sets register C
    to 1 if register A is greater than value B.
    Otherwise, register C is set to 0.
  gtrr (greater-than register/register) sets register C
    to 1 if register A is greater than register B.
    Otherwise, register C is set to 0.

Equality testing:
  eqir (equal immediate/register) sets register C to 1
    if value A is equal to register B. Otherwise, register C is set to 0.
  eqri (equal register/immediate) sets register C to 1
    if register A is equal to value B. Otherwise, register C is set to 0.
  eqrr (equal register/register) sets register C to 1
    if register A is equal to register B. Otherwise, register C is set to 0.

Unfortunately, while the manual gives the name of each opcode,
it doesn't seem to indicate the number.
However, you can monitor the CPU to see the contents of the registers
before and after instructions are executed to try to work them out.
Each opcode has a number from 0 through 15, but the manual doesn't
say which is which. For example, suppose you capture the following sample:

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

This sample shows the effect of the instruction 9 2 1 2 on the registers.
Before the instruction is executed,
register 0 has value 3,
register 1 has value 2, and
registers 2 and 3 have value 1.
After the instruction is executed, register 2's value becomes 2.

The instruction itself, 9 2 1 2, means that opcode 9 was executed with
A=2, B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above,
but only three of them behave in a way that would cause
the result shown in the sample:
  Opcode 9 could be mulr: register 2 (which has a value of 1)
    times register 1 (which has a value of 2) produces 2,
    which matches the value stored in the output register, register 2.
  Opcode 9 could be addi: register 2 (which has a value of 1)
    plus value 1 produces 2, which matches the value stored
    in the output register, register 2.
  Opcode 9 could be seti: value 2 matches the value stored in the
    output register, register 2; the number given for B is irrelevant.

None of the other opcodes produce the result captured in the sample.
Because of this, the sample above behaves like three opcodes.

You collect many of these samples (the first section of your puzzle input).
The manual also includes a small test program (the second section of your puzzle input)
 - you can ignore it for now.

Ignoring the opcode numbers, how many samples in
your puzzle input behave like three or more opcodes?

--- Part Two ---
Using the samples you collected,
work out the number of each opcode and execute the test program
(the second section of your puzzle input).
What value is contained in register 0 after executing the test program?
 */
import collection.mutable.{Map => MMap}

object DataDefs:
  type Register = Int
  type State = Seq[Register]

  enum OpType:
    case Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr,
      Eqir, Eqri, Eqrr
  import OpType.*

  case class Op(op: OpType, code: Int, a: Int, b: Int, c: Int):
    def execute(state: State): State = op match
      case Addr => state.updated(c, state(a) + state(b))
      case Addi => state.updated(c, state(a) + b)
      case Mulr => state.updated(c, state(a) * state(b))
      case Muli => state.updated(c, state(a) * b)
      case Banr => state.updated(c, state(a) & state(b))
      case Bani => state.updated(c, state(a) & b)
      case Borr => state.updated(c, state(a) | state(b))
      case Bori => state.updated(c, state(a) | b)
      case Setr => state.updated(c, state(a))
      case Seti => state.updated(c, a)
      case Gtir => state.updated(c, if a > b then 1 else 0)
      case Gtri => state.updated(c, if state(a) > b then 1 else 0)
      case Gtrr => state.updated(c, if state(a) > state(b) then 1 else 0)
      case Eqir => state.updated(c, if a == state(b) then 1 else 0)
      case Eqri => state.updated(c, if state(a) == b then 1 else 0)
      case Eqrr => state.updated(c, if state(a) == state(b) then 1 else 0)

  object Op:
    def generateAll(code: Int, a: Int, b: Int, c: Int) =
      OpType.values.toSeq.map(op => Op(op, code, a, b, c))

  case class Sample(before: State, ops: Seq[Op], after: State):
    def possibleOps = ops.filter(_.execute(before) == after)
    def countOps = possibleOps.size

object Parsing:
  import DataDefs.*, Op.*

  def parseOps(line: String) = line match
    case s"$code $a $b $c" => Op.generateAll(code.toInt, a.toInt, b.toInt, c.toInt)

  def parseState(line: String) = line match
    case s"Before: [$reg0, $reg1, $reg2, $reg3]" =>
      Seq(reg0.toInt, reg1.toInt, reg2.toInt, reg3.toInt)
    case s"After:  [$reg0, $reg1, $reg2, $reg3]" =>
      Seq(reg0.toInt, reg1.toInt, reg2.toInt, reg3.toInt)

  def parseBlock(block: String) =
    val Seq(before, ops, after) = block.split("\n").toSeq
    Sample(parseState(before), parseOps(ops), parseState(after))

  def parse(blocks: String) = blocks.split("\n\n").toSeq.map(parseBlock)

  def parseOpWithCode(map: MMap[Int, OpType])(line: String) = line match
    case s"$code $a $b $c" => Op(map(code.toInt), code.toInt, a.toInt, b.toInt, c.toInt)

  def parseOpsWithCodes(map: MMap[Int, OpType])(lines: Seq[String]) =
    lines map parseOpWithCode(map)

object Solving:
  import DataDefs.*

  def solve1(lines: String) = Parsing
    .parse(lines)
    .count(_.countOps >= 3)

  def figureOut(samples: Seq[Sample]) =
    var possible = samples.map(_.possibleOps) // .sortBy(_.size)
    var soFar = MMap[Int, OpType]()

    while possible.exists(_.size == 1) do
      val (smallest, rest) = possible.partition(_.size == 1)
      val newOps = smallest.map(_.head).map(op => op.code -> op.op)
      soFar ++= newOps
      possible = rest.map: ops =>
        ops.filterNot: op =>
          soFar.contains(op.code) || soFar.values.toSeq.contains(op.op)
    soFar

  def solve2(blocks: String, machine: Seq[String]) =
    val samples = Parsing.parse(blocks)
    val map = figureOut(samples)
    val ops = Parsing.parseOpsWithCodes(map)(machine)
    val finalState = ops.foldLeft(Seq(0, 0, 0, 0))((state, op) => op.execute(state))
    finalState.head

object Testing:
  lazy val lines = os.read(os.pwd / "2018" / "16" / "16.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
// Testing.result1 // part 1: 1

object Main:
  lazy val lines1 = os.read(os.pwd / "2018" / "16" / "16.input.txt")
  lazy val lines2 = os.read.lines(os.pwd / "2018" / "16" / "16.input.2.txt")
  lazy val result1 = Solving.solve1(lines1)
  lazy val result2 = Solving.solve2(lines1, lines2)
// Main.result1 // part 1: 567
// Main.result2 // part 2: 610

// Solving.figureOut(Parsing.parse(Main.lines1))
// Gtir -> 0,
// Setr -> 1
// Bori -> 2,
// Gtrr -> 3,
// Gtri -> 4,
// Eqir -> 5,
// Seti -> 6,
// Eqri -> 7,
// Eqrr -> 8,
// Borr -> 9,
// Addr -> 10,
// Mulr -> 11,
// Bani -> 12
// Muli -> 13,
// Banr -> 14,
// Addi -> 15,
