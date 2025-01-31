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
    var state  = State(instrs)
    for _ <- 0 until 1000000 do state = state.handleCurrentInst
    state.outs

object Main:
  lazy val file  = os.pwd / "2016" / "25" / "25.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
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
