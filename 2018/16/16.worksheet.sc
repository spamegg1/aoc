import collection.mutable.{Map => MMap}

object DataDefs:
  type Register = Int
  type State    = Seq[Register]

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
    def countOps    = possibleOps.size

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
    var soFar    = MMap[Int, OpType]()

    while possible.exists(_.size == 1) do
      val (smallest, rest) = possible.partition(_.size == 1)
      val newOps           = smallest.map(_.head).map(op => op.code -> op.op)
      soFar ++= newOps
      possible = rest.map: ops =>
        ops.filterNot: op =>
          soFar.contains(op.code) || soFar.values.toSeq.contains(op.op)
    soFar

  def solve2(blocks: String, machine: Seq[String]) =
    val samples    = Parsing.parse(blocks)
    val map        = figureOut(samples)
    val ops        = Parsing.parseOpsWithCodes(map)(machine)
    val finalState = ops.foldLeft(Seq(0, 0, 0, 0))((state, op) => op.execute(state))
    finalState.head

object Test:
  lazy val file  = os.pwd / "2018" / "16" / "16.test.input.txt"
  lazy val lines = os.read(file)
  lazy val res1  = Solving.solve1(lines)
// Test.res1 // part 1: 1

object Main:
  lazy val file1  = os.pwd / "2018" / "16" / "16.input.1.txt"
  lazy val file2  = os.pwd / "2018" / "16" / "16.input.2.txt"
  lazy val lines1 = os.read(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines1, lines2)
// Main.res1 // part 1: 567
// Main.res2 // part 2: 610

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
