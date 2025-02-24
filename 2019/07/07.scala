package aoc2019.day07

object DataDefs:
  val powers = Map(1 -> 100, 2 -> 1000)

  enum State:
    case Initial, Running, Halted
    case Output(value: Int)
  import State.*

  case class Cpu(ip: Int, code: Seq[Int], in: Seq[Int], state: State):
    def next = code(ip) % 100 match
      case 1 => Cpu(ip + 4, write(3, read(1) + read(2)), in, Running) // Add
      case 2 => Cpu(ip + 4, write(3, read(1) * read(2)), in, Running) // Multiply
      case 3 => Cpu(ip + 2, write(1, in.head), in.tail, Running)      // Read
      case 4 => Cpu(ip + 2, code, in, Output(read(1)))                // Write
      case 5  => Cpu(if read(1) != 0 then read(2) else ip + 3, code, in, Running) // jt
      case 6  => Cpu(if read(1) == 0 then read(2) else ip + 3, code, in, Running) // jf
      case 7  => Cpu(ip + 4, write(3, if read(1) < read(2) then 1 else 0), in, Running)
      case 8  => Cpu(ip + 4, write(3, if read(1) == read(2) then 1 else 0), in, Running)
      case 99 => Cpu(ip, code, in, Halted)                                        // Halt

    private def read(offset: Int): Int = (code(ip) / powers(offset)) % 10 match
      case 0 => code(code(ip + offset))
      case 1 => code(ip + offset)

    def write(offset: Int, value: Int) = code.updated(code(ip + offset), value)
    def withIn(next: Int*): Cpu        = copy(in = in.appendedAll(next))
    def nextOut: Cpu = Iterator.iterate(next)(_.next).dropWhile(_.state == Running).next()

    def allOut: Seq[Int] =
      val output = Iterator.iterate(this)(_.nextOut).takeWhile(_.state != Halted)
      output.toSeq.map(_.state).collect { case Output(value) => value }
  end Cpu

  object Cpu:
    def apply(code: Seq[Int]) = new Cpu(0, code, Seq(), Initial)

object Parsing:
  def parse(line: String) = line.split(",").map(_.toInt).toSeq

object Solving:
  import DataDefs.*, State.*

  def solve1(line: String) =
    val code = Parsing.parse(line)
    (0 to 4).permutations
      .map: perm =>
        perm
          .foldLeft(0): (total, next) =>
            Cpu(code)
              .withIn(next, total)
              .allOut
              .last
      .max

  @annotation.tailrec
  def helper(prevOut: Int)(amps: Seq[Cpu]): Int =
    val nextAmp = amps.head.withIn(prevOut).nextOut
    nextAmp.state match
      case Output(nextOut) => helper(nextOut)(amps.tail :+ nextAmp)
      case _               => prevOut

  def solve2(line: String) =
    val code = Parsing.parse(line)
    (5 to 9)
      .map: phase =>
        Cpu(code).withIn(phase)
      .permutations
      .map(helper(0))
      .max

object Test:
  lazy val file1 = os.pwd / "2019" / "07" / "07.test.1.txt"
  lazy val file2 = os.pwd / "2019" / "07" / "07.test.2.txt"
  lazy val file3 = os.pwd / "2019" / "07" / "07.test.3.txt"
  lazy val files = Seq(file1, file2, file3)
  lazy val lines = files.map(os.read.lines(_).head)
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = Solving.solve2(lines.head)

object Main:
  lazy val file = os.pwd / "2019" / "07" / "07.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 43210,54321,65210
  println(Test.res2) // part 2: 98765
  println(Main.res1) // part 1: 24625
  println(Main.res2) // part 2: 36497698
