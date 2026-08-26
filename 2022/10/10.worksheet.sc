object DataDefs:
  val CRT   = 40
  val Start = 20

  case class Op(value: Int, cycles: Int):
    def next = copy(cycles = cycles - 1)

  case class State(ops: List[Op], xReg: Int = 1, cycles: Int = 1):
    def strength = cycles * xReg
    def next = ops match
      case Op(v, 0) :: rest => State(rest, xReg + v, cycles + 1)
      case op :: rest       => State(op.next :: rest, xReg, cycles + 1)
      case Nil              => this

object Parsing:
  import DataDefs.Op

  def parseLine(line: String) = line match
    case s"addx $value" => Op(value.toInt, 1)
    case "noop"         => Op(0, 0)

  def parse(lines: Seq[String]) = lines.map(parseLine).toList

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val ops   = Parsing.parse(lines)
    var state = State(ops)
    var total = 0
    while state.ops.nonEmpty do
      state = state.next
      if (state.cycles - Start) % CRT == 0 then total += state.strength
    total

  def solve2(lines: Seq[String]) =
    val ops   = Parsing.parse(lines)
    var state = State(ops)
    var total = List[Char]()
    while state.ops.nonEmpty do
      val pixel = state.cycles - 1
      val char  = if math.abs(state.xReg - pixel % CRT) <= 1 then '#' else '.'
      total ::= char
      state = state.next
    total.reverse
      .grouped(40)
      .map(_.mkString)
      .mkString("\n")

object Test:
  val file  = os.pwd / "2022" / "10" / "10.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 13140
// Test.res2 // part 2:
// ##..##..##..##..##..##..##..##..##..##..
// ###...###...###...###...###...###...###.
// ####....####....####....####....####....
// #####.....#####.....#####.....#####.....
// ######......######......######......####
// #######.......#######.......#######.....

object Main:
  val file  = os.pwd / "2022" / "10" / "10.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 17380
// Main.res2 // part 2: FGCUZREC
// ####..##...##..#..#.####.###..####..##..
// #....#..#.#..#.#..#....#.#..#.#....#..#.
// ###..#....#....#..#...#..#..#.###..#....
// #....#.##.#....#..#..#...###..#....#....
// #....#..#.#..#.#..#.#....#.#..#....#..#.
// #.....###..##...##..####.#..#.####..##..
