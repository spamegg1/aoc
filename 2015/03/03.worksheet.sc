/*
--- Day 3: Perfectly Spherical Houses in a Vacuum ---
Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and
then an elf at the North Pole calls him via radio and tells him where to move
next. Moves are always exactly one house to the north (^), south (v), east (>),
or west (<). After each move, he delivers another present to the house at his
new location.

However, the elf back at the north pole has had a little too much eggnog, and so
his directions are a little off, and Santa ends up visiting some houses more
than once. How many houses receive at least one present?

For example:
> delivers to 2 houses: one at the starting location, and one to the east.
^>v< delivers presents to 4 houses in a square,
including twice to the house at his starting/ending location.
^v^v^v^v^v delivers a bunch of presents to some very lucky children at 2 houses.

--- Part Two ---
The next year, to speed up the process, Santa creates a robot
version of himself Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the
same starting house), then take turns moving based on instructions from the elf,
who is eggnoggedly reading from the same script as the previous year.

This year, how many houses receive at least one present?

For example:
  ^v delivers presents to 3 houses, because Santa goes north,
    and then Robo-Santa goes south.
  ^>v< now delivers presents to 3 houses,
    and Santa and Robo-Santa end up back where they started.
  ^v^v^v^v^v now delivers presents to 11 houses,
    with Santa going one direction and Robo-Santa going the other.
 */
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

object Testing:
  lazy val file    = os.pwd / "2015" / "03" / "03.test.input.txt"
  lazy val line    = os.read(file)
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Testing.result1 // part 1: 5
// Testing.result2 // part 2: 13

object Main:
  lazy val file    = os.pwd / "2015" / "03" / "03.input.txt"
  lazy val line    = os.read(file)
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part 1: 2572
// Main.result2 // part 2: 2631
