object DataDefs:
  type Offset = Int

  enum Register:
    case A, B
  import Register.*

  extension (s: String)
    def toRegister = s match
      case "a" => A
      case "b" => B

  enum Instr:
    case Hlf(reg: Register)
    case Tpl(reg: Register)
    case Inc(reg: Register)
    case Jmp(offset: Offset)
    case Jie(reg: Register, offset: Offset)
    case Jio(reg: Register, offset: Offset)
  import Instr.*

  case class State(ptr: Int, a: Int, b: Int)(using instrs: Seq[Instr]):
    lazy val halted = ptr < 0 || ptr >= instrs.size

    def updateRegister(reg: Register, fun: Int => Int): State = reg match
      case A => copy(ptr = ptr + 1, a = fun(a))
      case B => copy(ptr = ptr + 1, b = fun(b))

    def updatePointer(reg: Register, fun: Int => Boolean, offset: Int): State =
      val check = reg match
        case A => fun(a)
        case B => fun(b)
      copy(ptr = ptr + (if check then offset else 1))

    def nextState(instr: Instr): State = instr match
      case Hlf(reg)         => updateRegister(reg, State.half)
      case Tpl(reg)         => updateRegister(reg, State.triple)
      case Inc(reg)         => updateRegister(reg, State.incr)
      case Jmp(offset)      => copy(ptr = ptr + offset)
      case Jie(reg, offset) => updatePointer(reg, State.isEven, offset)
      case Jio(reg, offset) => updatePointer(reg, State.isOne, offset)

    def next = nextState(instrs(ptr))

  object State:
    val half   = (x: Int) => x / 2
    val triple = (x: Int) => x * 3
    val incr   = (x: Int) => x + 1
    val isEven = (x: Int) => x % 2 == 0
    val isOne  = (x: Int) => x == 1

object Parsing:
  import DataDefs.*, Instr.*, Register.*

  def parseLine(line: String): Instr = line match
    case s"hlf $reg"          => Hlf(reg.toRegister)
    case s"tpl $reg"          => Tpl(reg.toRegister)
    case s"inc $reg"          => Inc(reg.toRegister)
    case s"jmp $offset"       => Jmp(offset.toInt)
    case s"jie $reg, $offset" => Jie(reg.toRegister, offset.toInt)
    case s"jio $reg, $offset" => Jio(reg.toRegister, offset.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(ptr: Int, a: Int, b: Int)(lines: Seq[String]) =
    given Seq[Instr] = Parsing.parse(lines)
    var state        = State(ptr, a, b)
    while !state.halted do state = state.next
    state

  val solve1 = solve(0, 0, 0)
  val solve2 = solve(0, 1, 0)

object Test:
  lazy val file  = os.pwd / "2015" / "23" / "23.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve1(lines).a
// Test.res // part 1: 2

object Main:
  lazy val file  = os.pwd / "2015" / "23" / "23.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines).b
  lazy val res2  = Solving.solve2(lines).b
// Main.res1 // part 1: 307
// Main.res2 // part 2: 160
