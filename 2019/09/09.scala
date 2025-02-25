package aoc2019.day09

object DataDefs:
  val powers = Map(1 -> 100, 2 -> 1000, 3 -> 10000)
  type Memory = Map[Long, Long]

  enum State:
    case Init, Run, Halt
    case Out(value: Long)
    def out = this match
      case Out(value) => value
      case _          => -1L
  import State.*

  case class Cpu(ptr: Long, relBase: Long, mem: Memory, in: Seq[Long], state: State):
    private def read(offset: Int): Long = (mem(ptr) / powers(offset)) % 10 match
      case 0 => mem(mem(ptr + offset))
      case 1 => mem(ptr + offset)
      case 2 => mem(relBase + mem(ptr + offset))

    private def write(offset: Int, value: Long): Map[Long, Long] =
      (mem(ptr) / powers(offset)) % 10 match
        case 0 => mem.updated(mem(ptr + offset), value)
        case 2 => mem.updated(relBase + mem(ptr + offset), value)

    def next: Cpu = mem(ptr) % 100 match
      case 1 => copy(ptr = ptr + 4, mem = write(3, read(1) + read(2)), state = Run) // Add
      case 2 => copy(ptr = ptr + 4, mem = write(3, read(1) * read(2)), state = Run) // Mul
      case 3 => copy(ptr = ptr + 2, mem = write(1, in.head), in = in.tail, state = Run)
      case 4 => copy(ptr = ptr + 2, state = Out(read(1))) // Write
      case 5 => copy(ptr = if read(1) != 0 then read(2) else ptr + 3, state = Run) // jit
      case 6 => copy(ptr = if read(1) == 0 then read(2) else ptr + 3, state = Run) // jif
      case 7 =>
        val newMem = write(3, if read(1) < read(2) then 1 else 0)
        copy(ptr = ptr + 4, mem = newMem, state = Run) // Less than
      case 8 =>
        val newMem = write(3, if read(1) == read(2) then 1 else 0)
        copy(ptr = ptr + 4, mem = newMem, state = Run) // Equals
      case 9  => copy(ptr = ptr + 2, relBase = relBase + read(1), state = Run) // Rel base
      case 99 => copy(state = Halt)                                            // Halt

    def withIn(next: Long*): Cpu = copy(in = next)
    def nextOut: Cpu = Iterator.iterate(next)(_.next).dropWhile(_.state == Run).next()
    def allOut: Seq[Long] =
      val output = Iterator.iterate(this)(_.nextOut).takeWhile(_.state != Halt)
      output.toSeq.map(_.state).collect { case Out(value) => value }

  object Cpu:
    def apply(in: Seq[Long]): Cpu =
      val mem = in.zipWithIndex.map((value, index) => index.toLong -> value)
      new Cpu(0, 0, mem.toMap.withDefaultValue(0), Seq(), Init)

object Parsing:
  def parse(line: String) = line.split(",").map(_.toLong).toSeq

object Solving:
  import DataDefs.*

  def solve(in: Long)(line: String) = Cpu(Parsing.parse(line)).withIn(in).allOut.last
  val solve1                        = solve(1L)
  val solve2                        = solve(2L)

object Test:
  lazy val file  = os.pwd / "2019" / "09" / "09.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1

object Main:
  lazy val file = os.pwd / "2019" / "09" / "09.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 99, 1219070632396864, 1125899906842624
  println(Main.res1) // part 1: 3839402290
  println(Main.res2) // part 2: 35734
