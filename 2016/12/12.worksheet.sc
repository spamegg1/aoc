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
    var state  = State(instrs)
    while !state.halted do state = state.handleInstr(instrs(state.ptr))
    state.regs(Reg.A)

object Test:
  lazy val file  = os.pwd / "2016" / "12" / "12.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
// Test.res // part 1: 42

object Main:
  lazy val file  = os.pwd / "2016" / "12" / "12.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
// Main.res // part 1: 318117, part 2: 9227771
