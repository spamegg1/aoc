object DataDefs:
  enum Angle:
    case Ninety, OneEighty, TwoSeventy

    def rightToLeft = this match
      case Ninety     => TwoSeventy
      case OneEighty  => OneEighty
      case TwoSeventy => Ninety
  import Angle.*

  extension (s: String)
    def toAngle = s match
      case "90"  => Ninety
      case "180" => OneEighty
      case _     => TwoSeventy

  enum Action:
    case N(by: Int)
    case W(by: Int)
    case S(by: Int)
    case E(by: Int)
    case L(angle: Angle)
    case R(angle: Angle)
    case F(by: Int)
  import Action.*

  extension (c: Char)
    def toAction(number: String) = c match
      case 'N' => N(number.toInt)
      case 'W' => W(number.toInt)
      case 'S' => S(number.toInt)
      case 'E' => E(number.toInt)
      case 'L' => L(number.toAngle)
      case 'R' => R(number.toAngle)
      case 'F' => F(number.toInt)

  enum Dir:
    case North, West, South, East

    def turn(action: Action): Dir = action match
      case L(angle) => Dir.fromOrdinal((ordinal + angle.ordinal + 1) % 4)
      case R(angle) => Dir.fromOrdinal((ordinal + 3 - angle.ordinal) % 4)
      case _        => this
  import Dir.*

  case class Ship(x: Int, y: Int, facing: Dir): // part 1
    private def forward(by: Int) = facing match
      case North => Ship(x, y + by, facing)
      case West  => Ship(x - by, y, facing)
      case South => Ship(x, y - by, facing)
      case East  => Ship(x + by, y, facing)

    def take(action: Action): Ship = action match
      case N(by) => Ship(x, y + by, facing)
      case W(by) => Ship(x - by, y, facing)
      case S(by) => Ship(x, y - by, facing)
      case E(by) => Ship(x + by, y, facing)
      case F(by) => forward(by)
      case _     => Ship(x, y, facing.turn(action))

    lazy val manhattan = math.abs(x) + math.abs(y)

  // these coordinates are relative to the Ship.
  case class Waypoint(x: Int, y: Int, ship: Ship): // part 2
    def rotateLeft(angle: Angle): Waypoint = angle match
      case Ninety     => Waypoint(-y, x, ship)
      case OneEighty  => Waypoint(-x, -y, ship)
      case TwoSeventy => Waypoint(y, -x, ship)

    def rotateRight(angle: Angle): Waypoint = rotateLeft(angle.rightToLeft)

    def take(action: Action): Waypoint = action match
      case N(by)    => Waypoint(x, y + by, ship)
      case W(by)    => Waypoint(x - by, y, ship)
      case S(by)    => Waypoint(x, y - by, ship)
      case E(by)    => Waypoint(x + by, y, ship)
      case L(angle) => rotateLeft(angle)
      case R(angle) => rotateRight(angle)
      case F(by)    => Waypoint(x, y, ship.take(E(x * by)).take(N(y * by)))

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Action        = line.head.toAction(line.tail)
  def parse(lines: Seq[String]): Seq[Action] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val actions = Parsing.parse(lines)
    var ship    = Ship(0, 0, Dir.East) // starts facing east
    for action <- actions do ship = ship.take(action)
    ship.manhattan

  def solve2(lines: Seq[String]) =
    val actions  = Parsing.parse(lines)
    val ship     = Ship(0, 0, Dir.East)
    var waypoint = Waypoint(10, 1, ship) // 10 east, 1 north
    for action <- actions do waypoint = waypoint.take(action)
    waypoint.ship.manhattan

object Test:
  lazy val file  = os.pwd / "2020" / "12" / "12.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 25
// Test.res2 // part 2: 286

object Main:
  lazy val file  = os.pwd / "2020" / "12" / "12.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 2458
// Main.res2 // part 2: 145117
