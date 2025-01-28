/*
--- Day 18: Duet ---
You discover a tablet containing some strange assembly code labeled simply "Duet".
Rather than bother the sound card with it, you decide to run the code yourself.
Unfortunately, you don't see any documentation, so you're left to figure out
what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are
each named with a single letter and that can each hold a single integer.
You suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what they do.
Here's what you determine:
  snd X plays a sound with a frequency equal to the value of X.
  set X Y sets register X to the value of Y.
  add X Y increases register X by the value of Y.
  mul X Y sets register X to the res of multiplying the value contained in
    register X by the value of Y.
  mod X Y sets register X to the remainder of dividing the value contained in
    register X by the value of Y (that is, it sets X to the res of X modulo Y).
  rcv X recovers the frequency of the last sound played,
    but only when the value of X is not zero.
    (If it is zero, the command does nothing.)
  jgz X Y jumps with an offset of the value of Y,
    but only if the value of X is greater than zero.
    (An offset of 2 skips the next instruction,
    an offset of -1 jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a number.
The value of a register is the integer it contains; the value of a number is that number.

After each jump instruction, the program continues with the instruction to which the jump
jumped. After any other instruction, the program continues with the next instruction.
Continuing (or jumping) off either end of the program terminates it.

For example:
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
  The first four instructions set a to 1, add 2 to it = 3, square it 9,
    and then set it to itself modulo 5, resing in a value of 4.
  Then, a sound with frequency 4 (the value of a) is played.
  After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be
    skipped (rcv because a is 0, and jgz because a is not greater than 0).
  Finally, a is set to 1, causing the next jgz instruction to activate,
    jumping back two instructions to another jump,
    which jumps again to the rcv,
    which ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound played is 4

What is the value of the recovered frequency
(the value of the most recently played sound)
the first time a rcv instruction is executed with a non-zero value?

--- Part Two ---
As you congratulate yourself for a job well done,
you notice that the documentation has been on the back of the tablet this entire time.
While you actually got most of the instructions correct, there are a few key differences.
This assembly code isn't about sound at all - it's meant to be run twice at the same time.

Each running copy of the program has its own set of registers
and follows the code independently - in fact, the programs
don't even necessarily run at the same speed.
To coordinate, they use the send (snd) and receive (rcv) instructions:
  snd X sends the value of X to the other program.
    These values wait in a queue until that program is ready to receive them.
    Each program has its own message queue,
    so a program can never receive a message it sent.
  rcv X receives the next value and stores it in register X.
    If no values are in the queue,
    the program waits for a value to be sent to it.
    Programs do not continue to the next instruction until they have received a value.
    Values are received in the order they are sent.

Each program also has its own program ID (one 0 and the other 1);
the register p should begin with this value. For example:

snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d

Both programs begin by sending three values to the other.
Program 0 sends 1, 2, 0; program 1 sends 1, 2, 1.
Then, each program receives a value (both 1) and stores it in a,
receives another value (both 2) and stores it in b,
and then each receives the program ID of the other program
(program 0 receives 1; program 1 receives 0) and stores it in c.
Each program now sees a different value in its own copy of register c.

NOTE: there are no "snd p" instructions in my puzzle input!
But p register should still be initialized 0 and 1.

Finally, both programs try to rcv a fourth time,
but no data is waiting for either of them,
and they reach a deadlock. When this happens, both programs terminate.

It should be noted that it would be equally valid for the programs
to run at different speeds; for example, program 0 might have sent
all three values and then stopped at the first rcv before program 1
executed even its first instruction.

Once both of your programs have terminated
(regardless of what caused them to do so),
how many times did program 1 send a value?
 */
object DataDefs:
  import collection.immutable.Queue

  type Reg = Char
  type Freq = Long

  extension (value: Reg | Freq) // union types yay! They are like enums! Kinda?
    def get(regs: Map[Reg, Freq]): Freq = value match
      case x: Reg  => regs(x) // replace _ with x, solves the type issues!
      case x: Freq => x

  enum Instr:
    case Snd(x: Reg | Freq)
    case Set(x: Reg, y: Reg | Freq)
    case Add(x: Reg, y: Reg | Freq)
    case Mul(x: Reg, y: Reg | Freq)
    case Mod(x: Reg, y: Reg | Freq)
    case Rcv(x: Reg)
    case Jgz(x: Reg | Freq, y: Reg | Freq)
  import Instr.*

  case class Program(
      regs: Map[Reg, Freq],
      size: Int,
      ptr: Int = 0,
      finished: Boolean = false,
      played: List[Freq] = Nil, // part 1
      recovered: Freq = 0, // part 1
      sent: Int = 0, // part 2
      queue: Queue[Freq] = Queue() // part 2
  ):
    def isDone = ptr < 0 || size <= ptr // part 2
    def deadlocked(instr: Instr) = instr match // part 2
      case Rcv(x) => queue.isEmpty
      case _      => false

    def rcvFrom(value: Freq) = copy(queue = queue.enqueue(value)) // part 2

    def deq(x: Reg) = // part 2
      val (freq, rest) = queue.dequeue
      copy(regs = regs.updated(x, freq), queue = rest, ptr = ptr + 1)

    def run(instr: Instr) = instr match
      case Snd(x) => // plays a sound with a frequency equal to the value of X.
        copy(played = x.get(regs) :: played, sent = sent + 1, ptr = ptr + 1)
      case Set(x, y) => // sets register X to the value of Y.
        copy(regs = regs.updated(x, y.get(regs)), ptr = ptr + 1)
      case Add(x, y) => // increases register X by the value of Y.
        copy(regs = regs.updated(x, regs(x) + y.get(regs)), ptr = ptr + 1)
      case Mul(x, y) => // sets register X to the res of multiplying X and Y
        copy(regs = regs.updated(x, regs(x) * y.get(regs)), ptr = ptr + 1)
      case Mod(x, y) => // sets register X to the res of X modulo Y
        copy(regs = regs.updated(x, regs(x) % y.get(regs)), ptr = ptr + 1)
      case Rcv(x) => // recovers the frequency of the last sound played, if X != 0
        if regs(x) != 0 then copy(ptr = ptr + 1, finished = true, recovered = played.head)
        else copy(ptr = ptr + 1)
      case Jgz(x, y) => // jumps with an offset of the value of Y, if X > 0.
        copy(ptr = ptr + (if x.get(regs) > 0L then y.get(regs).toInt else 1))

  object Program:
    private lazy val startRegs = ('a' to 'z').map(_ -> 0L).toMap
    def apply(size: Int) = new Program(startRegs, size)
    def prog1(size: Int) = new Program(startRegs.updated('p', 1L), size)

  enum Turn:
    case First, Second
    def next = this match
      case First  => Second
      case Second => First
  import Turn.*

  case class TwoPrograms(p0: Program, p1: Program, turn: Turn, instrs: Seq[Instr]):
    def p0cannotRun = p0.isDone || p0.deadlocked(instrs(p0.ptr))
    def p1cannotRun = p1.isDone || p1.deadlocked(instrs(p1.ptr))
    def canRun = !p0cannotRun || !p1cannotRun

    def run: TwoPrograms = turn match // assume canRun = true
      case First =>
        if p0.isDone then copy(turn = turn.next)
        else
          instrs(p0.ptr) match
            case Snd(x) =>
              copy(p0 = p0.run(Snd(x)), p1 = p1.rcvFrom(x.get(p0.regs)), turn = turn.next)
            case Rcv(x) =>
              if p0.deadlocked(Rcv(x)) then copy(turn = turn.next) // switch if deadlocked
              else copy(p0 = p0.deq(x), turn = turn.next)
            case instr => copy(p0 = p0.run(instr), turn = turn.next) // run p0, switch
      case Second =>
        if p1.isDone then copy(turn = turn.next)
        else
          instrs(p1.ptr) match
            case Snd(x) =>
              copy(p1 = p1.run(Snd(x)), p0 = p0.rcvFrom(x.get(p1.regs)), turn = turn.next)
            case Rcv(x) =>
              if p1.deadlocked(Rcv(x)) then copy(turn = turn.next) // switch if deadlocked
              else copy(p1 = p1.deq(x), turn = turn.next)
            case instr => copy(p1 = p1.run(instr), turn = turn.next) // run p1, switch

object Parsing:
  import DataDefs.*, Instr.*

  def parseLine(line: String): Instr = line match
    case s"snd $x" =>
      x.toLongOption match
        case None        => Snd(x.head)
        case Some(value) => Snd(value)

    case s"set $x $y" =>
      y.toLongOption match
        case None        => Set(x.head, y.head)
        case Some(value) => Set(x.head, value)
    case s"add $x $y" =>
      y.toLongOption match
        case None        => Add(x.head, y.head)
        case Some(value) => Add(x.head, value)
    case s"mul $x $y" =>
      y.toLongOption match
        case None        => Mul(x.head, y.head)
        case Some(value) => Mul(x.head, value)
    case s"mod $x $y" =>
      y.toLongOption match
        case None        => Mod(x.head, y.head)
        case Some(value) => Mod(x.head, value)
    case s"rcv $x" => Rcv(x.head)
    case s"jgz $x $y" =>
      (x.toLongOption, y.toLongOption) match
        case (None, None)         => Jgz(x.head, y.head)
        case (None, Some(value))  => Jgz(x.head, value)
        case (Some(value), None)  => Jgz(value, y.head)
        case (Some(v1), Some(v2)) => Jgz(v1, v2)

  def parse(lines: Seq[String]): Seq[Instr] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]): Freq =
    val instrs = Parsing.parse(lines)
    var program = Program(instrs.size)
    while !program.finished do program = program.run(instrs(program.ptr))
    program.recovered

  def solve2(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    val program0 = Program(instrs.size)
    val program1 = Program.prog1(instrs.size)
    var twoProgs = TwoPrograms(program0, program1, Turn.First, instrs)
    while twoProgs.canRun do twoProgs = twoProgs.run
    twoProgs.p1.sent

object Test:
  lazy val lines1 = os.read.lines(os.pwd / "2017" / "18" / "18.test.input.txt")
  lazy val lines2 = os.read.lines(os.pwd / "2017" / "18" / "18.test.input.2.txt")
  lazy val res1 = Solving.solve1(lines1)
  lazy val res2 = Solving.solve2(lines2)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 3

object Main:
  lazy val lines = os.read.lines(os.pwd / "2017" / "18" / "18.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 1187
// Main.res2 // part 2: 5969
