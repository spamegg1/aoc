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
    val (ipReg, ops) = Parsing.parse(lines)
    var state        = State(ipReg, 0, Seq(0, 0, 0, 0, 0, 0), ops, false)
    while !state.halted do state = state.next
    state.regs(0)

  def solve2(lines: Seq[String]) =
    val (ipReg, ops) = Parsing.parse(lines)
    var state        = State(ipReg, 0, Seq(0, 11, 10, 1, 10551394, 0), ops, false)
    var i            = 0
    while i < 100 do
      println(state.regs)
      state = state.next
      i += 1

object Test: // #ip 0
  lazy val file  = os.pwd / "2018" / "19" / "19.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
// Test.res1 // part 1: 7

object Main: // #ip 2
  lazy val file  = os.pwd / "2018" / "19" / "19.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1728
// Main.res2 // part 2: 18200448

// Code is calculating the sum of all divisors of a number.
// 10551394 keeps repeating
// its prime factorization is 10551394 = 2 x 7 x 167 x 4513
// so
// 1 +
// 2 + 7 + 167 + 4513 +
// 2*7 + 2*167 + 2*4513 + 7*167 + 7*4513 + 167*4513 +
// 2*7*167 + 2*7*4513 + 2*167*4513 + 7*167*4513 +
// 2*7*167*4513
