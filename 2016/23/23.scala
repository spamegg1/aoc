package aoc2016.day23

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
    case Add(reg1: Reg, reg2: Reg, reg3: Reg)
    case Mul(reg1: Reg, reg2: Reg, reg3: Reg)
    case Noop
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
      case Add(reg1, reg2, reg3)      => ???     // these 3 are all above tgl in input.
      case Mul(reg1, reg2, reg3)      => ???
      case Noop                       => ???
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
      case Add(reg1, reg2, reg3) =>
        copy(regs = regs.updated(reg3, regs(reg1) + regs(reg2)), ptr = ptr + 1)
      case Mul(reg1, reg2, reg3) =>
        copy(regs = regs.updated(reg3, regs(reg1) * regs(reg2)), ptr = ptr + 1)
      case Noop    => copy(ptr = ptr + 1)
      case Illegal => copy(ptr = ptr + 1)

    def handleCurrentInst = handleInstr(instrs(ptr))

  object State:
    def apply(instrs: Seq[Instr], a: Int) =
      new State(Map(A -> a, B -> 0, C -> 0, D -> 0), instrs, 0)

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
    case s"add $reg1 $reg2 $reg3" => Add(reg1.toReg, reg2.toReg, reg3.toReg)
    case s"mul $reg1 $reg2 $reg3" => Mul(reg1.toReg, reg2.toReg, reg3.toReg)
    case s"nop"                   => Noop

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String])(a: Int) =
    var state = State(Parsing.parse(lines), a)
    while !state.halted do state = state.handleCurrentInst
    state.regs(Reg.A)

  // For part 2, optimizations are applied directly to the input.
  // (Thankfully, the only tgl comes after both add and mul in the input.)
  // This is multiplication:
  // inc X       mul Y Z X
  // dec Y       cpy 0 Y
  // jnz Y -2 => cpy 0 Z
  // dec Z       nop
  // jnz Z -5    nop
  //
  // These are both addition:
  // inc X       add X Y X
  // dec Y    => cpy 0 Y
  // jnz Y -2    nop
  //
  // dec X       add X Y Y
  // inc Y    => cpy 0 X
  // jnz X -2    nop

object Test:
  lazy val file  = os.pwd / "2016" / "23" / "23.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)(7)

object Main:
  lazy val file1 = os.pwd / "2016" / "23" / "23.input.1.txt"
  lazy val file2 = os.pwd / "2016" / "23" / "23.input.2.txt" // optimized
  lazy val line1 = os.read.lines(file1)
  lazy val line2 = os.read.lines(file2)
  lazy val res1  = Solving.solve(line1)(7)
  lazy val res2  = Solving.solve(line2)(12)

@main
def run: Unit =
  println(Test.res)  // part 1: 3
  println(Main.res1) // part 1: 10584
  println(Main.res2) // part 2: 479007144
