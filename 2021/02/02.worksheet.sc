object DataDefs:
  enum Dir:
    case Forward, Up, Down
  import Dir.*

  extension (s: String)
    def toDir = s match
      case "forward" => Forward
      case "up"      => Up
      case "down"    => Down

  case class Move(dir: Dir, value: Int)

  case class Pos(horiz: Int, depth: Int, aim: Int):
    def go(move: Move): Pos = move.dir match
      case Forward => Pos(horiz + move.value, depth, aim)
      case Up      => Pos(horiz, depth - move.value, aim)
      case Down    => Pos(horiz, depth + move.value, aim)

    def go2(move: Move): Pos = move.dir match
      case Forward => Pos(horiz + move.value, depth + aim * move.value, aim)
      case Up      => Pos(horiz, depth, aim - move.value)
      case Down    => Pos(horiz, depth, aim + move.value)

    lazy val product = horiz * depth

  type Fun     = Pos => Move => Pos
  type Process = List[Move] => Pos => Pos

object Parsing:
  import DataDefs.*, Dir.*

  private def parseMove(line: String): Move = line match
    case s"$direction $value" => Move(direction.toDir, value.toInt)

  def parseMoves(lines: Seq[String]) = lines map parseMove

object Solving:
  import DataDefs.*, Dir.*

  @annotation.tailrec
  private def process(fun: Fun)(moves: List[Move])(pos: Pos): Pos = moves match
    case head :: next => process(fun)(next)(fun(pos)(head))
    case Nil          => pos

  private val start = Pos(0, 0, 0)

  def solve(proc: Process)(lines: Seq[String]): Int =
    val moves = Parsing.parseMoves(lines).toList
    proc(moves)(start).product

  val solve1 = solve(process(_.go))
  val solve2 = solve(process(_.go2))

object Test:
  lazy val file  = os.pwd / "2021" / "02" / "02.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 150
// Test.res2 // part 2: 900

object Main:
  lazy val file  = os.pwd / "2021" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1459206
// Main.res2 // part 2: 1320534480
