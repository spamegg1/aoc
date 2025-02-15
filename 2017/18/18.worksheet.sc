object DataDefs:
  import collection.immutable.Queue

  type Reg  = Char
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
      played: List[Freq] = Nil,    // part 1
      recovered: Freq = 0,         // part 1
      sent: Int = 0,               // part 2
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
    def apply(size: Int)       = new Program(startRegs, size)
    def prog1(size: Int)       = new Program(startRegs.updated('p', 1L), size)

  enum Turn:
    case First, Second
    def next = this match
      case First  => Second
      case Second => First
  import Turn.*

  case class TwoPrograms(p0: Program, p1: Program, turn: Turn, instrs: Seq[Instr]):
    def p0cannotRun = p0.isDone || p0.deadlocked(instrs(p0.ptr))
    def p1cannotRun = p1.isDone || p1.deadlocked(instrs(p1.ptr))
    def canRun      = !p0cannotRun || !p1cannotRun

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
    val instrs  = Parsing.parse(lines)
    var program = Program(instrs.size)
    while !program.finished do program = program.run(instrs(program.ptr))
    program.recovered

  def solve2(lines: Seq[String]) =
    val instrs   = Parsing.parse(lines)
    val program0 = Program(instrs.size)
    val program1 = Program.prog1(instrs.size)
    var twoProgs = TwoPrograms(program0, program1, Turn.First, instrs)
    while twoProgs.canRun do twoProgs = twoProgs.run
    twoProgs.p1.sent

object Test:
  lazy val file1  = os.pwd / "2017" / "18" / "18.test.input.txt"
  lazy val file2  = os.pwd / "2017" / "18" / "18.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 3

object Main:
  lazy val file  = os.pwd / "2017" / "18" / "18.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1187
// Main.res2 // part 2: 5969
