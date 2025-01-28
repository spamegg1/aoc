/*
--- Day 25: Clock Signal ---
You open the door and find yourself on the roof.
The city sprawls away from you for miles and miles.

There's not much time now - it's already Christmas,
but you're nowhere near the North Pole,
much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution.
After all, the sleigh doesn't need the stars, exactly;
it needs the timing data they provide, and you happen to
have a massive signal generator right here.

You connect the stars you have to your prototype computer,
connect that to the antenna, and begin the transmission.
Nothing happens.

You call the service number printed on the side of the antenna
and quickly explain the situation. "I'm not sure what kind of
equipment you have connected over there," he says,
"but you need a clock signal." You try to explain that this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer
knows how to read the data you're sending it. An endless,
alternating pattern of 0, 1, 0, 1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency
you would need to use for the data from the stars.
"There's no way it can! The only antenna we've installed capable
of that is on top of a top-secret Easter Bunny installation, and
you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code
(your puzzle input); it looks mostly compatible with code you worked on just recently.

This antenna code, being a signal generator, uses one extra instruction:
  out x transmits x (either an integer or the value of a register) as
  the next value for the clock signal.

The code takes a value (via register a) that describes the signal to generate,
but you're not sure how it's used.
You'll have to find the input to produce the right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a
and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?

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
    case JnzRegVal(notZero: Reg, offset: Int)
    case OutVal(value: Int)
    case OutReg(reg: Reg)
  import Instr.*

  case class State(regs: Map[Reg, Int], instrs: Seq[Instr], ptr: Int, outs: List[Int]):
    def halted = ptr < 0 || instrs.size <= ptr
    def handleInstr(instr: Instr): State = instr match
      case CopyValReg(value, to) => copy(regs = regs.updated(to, value), ptr = ptr + 1)
      case CopyRegReg(from, to) =>
        copy(regs = regs.updated(to, regs(from)), ptr = ptr + 1)
      case Inc(reg) => copy(regs = regs.updated(reg, regs(reg) + 1), ptr = ptr + 1)
      case Dec(reg) => copy(regs = regs.updated(reg, regs(reg) - 1), ptr = ptr + 1)
      case JnzValVal(notZero, offset) =>
        copy(ptr = ptr + (if notZero != 0 then offset else 1))
      case JnzRegVal(notZero, offset) =>
        copy(ptr = ptr + (if regs(notZero) != 0 then offset else 1))
      case OutVal(value: Int) => copy(outs = value :: outs, ptr = ptr + 1)
      case OutReg(reg: Reg)   => copy(outs = regs(reg) :: outs, ptr = ptr + 1)

    def handleCurrentInst = handleInstr(instrs(ptr))

  object State:
    def apply(instrs: Seq[Instr]) =
      new State(Map(A -> 10000, B -> 0, C -> 0, D -> 0), instrs, 0, Nil)

object Parsing:
  import DataDefs.*, Instr.*

  def parseLine(line: String): Instr = line match
    case s"inc $reg" => Inc(reg.toReg)
    case s"dec $reg" => Dec(reg.toReg)
    case s"cpy $regOrVal $reg" =>
      regOrVal.toIntOption match
        case None        => CopyRegReg(regOrVal.toReg, reg.toReg)
        case Some(value) => CopyValReg(value, reg.toReg)
    case s"jnz $regOrVal $offset" =>
      regOrVal.toIntOption match
        case None        => JnzRegVal(regOrVal.toReg, offset.toInt)
        case Some(value) => JnzValVal(value, offset.toInt)
    case s"out $regOrVal" =>
      regOrVal.toIntOption match
        case None        => OutReg(regOrVal.toReg)
        case Some(value) => OutVal(value)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    var state = State(instrs)
    for _ <- 0 until 1000000 do state = state.handleCurrentInst
    state.outs

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "25" / "25.input.txt")
  lazy val res = Solving.solve(lines)
// Main.res // part 1: 158
// Reverse engineering the code shows that it takes the initial value "a'
// then adds 4 * 643 to create a seed.
// This seed is then repeatedly bit shifted right by dividing by 2
// using an inefficient linear time loop.
// The remainder (the bit that drops off) is the output.
// This means that output sequence is simply the binary digits of "a" + 4 * 643
// in reverse, repeated over and over.
// To obtain the desired pattern we need the next highest binary number that has the
// pattern "101010.....". For this data that number is 2730 - 4 * 643 = 158.
