object DataDefs:
  type Register   = String
  type Registers  = Map[Register, Int]
  type Comparison = (Int, Int) => Boolean

  case class Condition(reg: Register, value: Int, comp: Comparison):
    def holds(regs: Registers) = comp(regs(reg), value)

  case class Instr(reg: Register, value: Int, cond: Condition):
    def process(regs: Registers): Registers =
      regs.updated(reg, regs(reg) + (if cond.holds(regs) then value else 0))

  object Registers:
    def apply(instrs: Seq[Instr]): Registers =
      val allRegisters = instrs.flatMap(instr => Seq(instr.reg, instr.cond.reg)).distinct
      allRegisters.map(reg => reg -> 0).toMap

  extension (s: String)
    def toComp: Comparison = s match
      case ">"  => _ > _
      case ">=" => _ >= _
      case "<"  => _ < _
      case "<=" => _ <= _
      case "==" => _ == _
      case "!=" => _ != _

    def incDec(value: Int): Int = s match
      case "inc" => value
      case "dec" => -value

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): Instr = line match
    case s"$writeTo $opr $write if $readFrom $comp $int" =>
      Instr(writeTo, opr.incDec(write.toInt), Condition(readFrom, int.toInt, comp.toComp))

  def parse(lines: Seq[String]): Seq[Instr] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val instrs    = Parsing.parse(lines)
    val registers = Registers(instrs)
    instrs.foldLeft(registers)((regs, instr) => instr.process(regs)).values.max

  def solve2(lines: Seq[String]) =
    val instrs    = Parsing.parse(lines)
    val registers = Registers(instrs)
    var maxSoFar  = 0
    val res = instrs.foldLeft(registers): (regs, instr) =>
      maxSoFar = math.max(maxSoFar, regs.values.max)
      instr.process(regs)
    maxSoFar

object Test:
  lazy val file  = os.pwd / "2017" / "08" / "08.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 1
// Test.res2 // part 2: 10

object Main:
  lazy val file  = os.pwd / "2017" / "08" / "08.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 5946
// Main.res2 // part 2: 6026
