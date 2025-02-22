package aoc2018.day21

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
      case Gtir => regs.updated(c, if a > regs(b) then 1 else 0)
      case Gtri => regs.updated(c, if regs(a) > b then 1 else 0)
      case Gtrr => regs.updated(c, if regs(a) > regs(b) then 1 else 0)
      case Eqir => regs.updated(c, if a == regs(b) then 1 else 0)
      case Eqri => regs.updated(c, if regs(a) == b then 1 else 0)
      case Eqrr => regs.updated(c, if regs(a) == regs(b) then 1 else 0)

  case class State(ipReg: Int, ptr: Int, regs: Seq[Reg], ops: Seq[Op], halted: Boolean):
    def next: State =
      try
        val op     = ops(ptr)
        val regs1  = regs.updated(ipReg, ptr.toLong)
        val regs2  = op.execute(regs1)
        val newPtr = regs2(ipReg) + 1
        val regs3  = regs2.updated(ipReg, newPtr)
        copy(regs = regs3, ptr = newPtr.toInt)
      catch case _: IndexOutOfBoundsException => copy(halted = true)

object Parsing:
  import DataDefs.*
  def parseLine(line: String) = line match
    case s"$op $a $b $c" => Op(op.toOp, a.toInt, b.toInt, c.toInt)

  def parse(lines: Seq[String]) =
    val ipReg = lines.head.last.asDigit
    val ops   = lines.tail map parseLine
    (ipReg, ops)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    // the only instr that refers to register 0 is on line 28.
    // it checks if reg2 == reg0, if yes, we exit program.
    // so we need to find the earliest that we can reach line 28.
    // then we can exit by setting register 0 to be equal to register 2.
    val (ipReg, ops) = Parsing.parse(lines)
    var state        = State(ipReg, 0, Seq(0L, 0L, 0L, 0L, 0L, 0L), ops, false)
    while state.ptr != 28 do state = state.next
    state.regs(2)

  def solve2(lines: Seq[String]) =
    import util.boundary, boundary.break
    // We keep track of all the values register 2 can have before repeating a cycle.
    // The last value of the cycle gives us the longest terminating execution.
    // We run the reverse-engineered code from ptr=5: seti 0 3 2 to ptr:27 seti 7 3 1
    var r2History = List[Long]()
    var r2        = 0L
    var r4        = 0L
    var r5        = 0L
    while !r2History.contains(r2) do
      r2History ::= r2
      r5 = 65536L | r2
      r2 = 4843319L
      boundary:
        while true do
          r4 = r5 & 255L
          r2 = (((r2 + r4) & 16777215L) * 65899L) & 16777215L
          if r5 >= 256L then r5 /= 256L else break()
    r2History.head

object Main:
  lazy val file  = os.pwd / "2018" / "21" / "21.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Main.res1) // part 1: 8797248
  println(Main.res2) // part 2: 3007673
