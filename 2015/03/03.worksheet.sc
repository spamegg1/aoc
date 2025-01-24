object DataDefs:
  enum Dir:
    case N, S, E, W
  import Dir.*

  extension (c: Char)
    def toDir: Dir = c match
      case '^' => N
      case 'v' => S
      case '>' => E
      case '<' => W

  case class Pos(x: Int, y: Int):
    def go(dir: Dir): Pos = dir match
      case N => Pos(x, y + 1)
      case S => Pos(x, y - 1)
      case E => Pos(x + 1, y)
      case W => Pos(x - 1, y)

object Parsing:
  import DataDefs.*
  def parse(line: String): List[Dir] = line.map(_.toDir).toList

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def processDirs(dirs: List[Dir])(current: Pos)(poses: Set[Pos]): Set[Pos] =
    dirs match
      case head :: next =>
        val nextPos = current.go(head)
        processDirs(next)(nextPos)(poses + nextPos)
      case Nil => poses

  private val start = Pos(0, 0)

  def solve1(line: String) = processDirs(Parsing.parse(line))(start)(Set(start)).size

  def solve2(line: String) =
    val dirs           = Parsing.parse(line)
    val (santa, robot) = dirs.zipWithIndex.partition((_, index) => index % 2 == 0)
    val santaHouses    = processDirs(santa.map(_._1))(start)(Set(start))
    val robotHouses    = processDirs(robot.map(_._1))(start)(Set(start))
    (santaHouses union robotHouses).size

object Test:
  lazy val file = os.pwd / "2015" / "03" / "03.test.input.txt"
  lazy val line = os.read(file)
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Test.res1 // part 1: 5
// Test.res2 // part 2: 13

object Main:
  lazy val file = os.pwd / "2015" / "03" / "03.input.txt"
  lazy val line = os.read(file)
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 2572
// Main.res2 // part 2: 2631
