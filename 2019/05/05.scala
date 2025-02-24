package aoc2019.day05

object DataDefs:
  val powers = Map(1 -> 100, 2 -> 1000)

  case class CPU(ptr: Int, code: Seq[Int], in: Seq[Int], out: List[Int], halt: Boolean):
    def mode(offset: Int): Int = (code(ptr) / powers(offset) % 10) match
      case 0 => code(code(ptr + offset))
      case 1 => code(ptr + offset)

    def next = (code(ptr) % 100) match
      case 1 => // Add
        val add      = mode(1) + mode(2)
        val nextCode = code.updated(code(ptr + 3), add)
        copy(ptr = ptr + 4, code = nextCode)
      case 2 => // Multiply
        val mul      = mode(1) * mode(2)
        val nextCode = code.updated(code(ptr + 3), mul)
        copy(ptr = ptr + 4, code = nextCode)
      case 3 => // Read
        val nextCode = code.updated(code(ptr + 1), in.head)
        copy(ptr = ptr + 2, code = nextCode, in = in.tail)
      case 4 => copy(ptr = ptr + 2, out = mode(1) :: out) // Write
      case 5 => // Jump if true
        val nextPtr = if mode(1) != 0 then mode(2) else ptr + 3
        copy(ptr = nextPtr)
      case 6 => // Jump if false
        val nextPtr = if mode(1) == 0 then mode(2) else ptr + 3
        copy(ptr = nextPtr)
      case 7 => // Less than
        val compare  = if mode(1) < mode(2) then 1 else 0
        val nextCode = code.updated(code(ptr + 3), compare)
        copy(ptr = ptr + 4, code = nextCode)
      case 8 => // Equals
        val compare  = if mode(1) == mode(2) then 1 else 0
        val nextCode = code.updated(code(ptr + 3), compare)
        copy(ptr = ptr + 4, code = nextCode)
      case 99 => copy(halt = true)

object Parsing:
  def parse(line: String): Seq[Int] = line.split(",").map(_.toInt).toSeq

object Solving:
  import DataDefs.*

  def solve(in: Int)(line: String): Int =
    var test = CPU(0, Parsing.parse(line), Seq(in), List(), false)
    while !test.halt do test = test.next
    test.out.head

  val solve1 = solve(1)
  val solve2 = solve(5)

object Test:
  lazy val file1 = os.pwd / "2019" / "05" / "05.test.input.1.txt"
  lazy val file2 = os.pwd / "2019" / "05" / "05.test.input.2.txt"
  lazy val line1 = os.read.lines(file1).head
  lazy val line2 = os.read.lines(file2).head
  lazy val res1  = Solving.solve1(line1)
  lazy val res2  = Solving.solve1(line2)

object Main:
  lazy val file = os.pwd / "2019" / "05" / "05.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 123456
  println(Test.res2) // part 2: 999
  println(Main.res1) // part 1: 9654885
  println(Main.res2) // part 2: 7079459
