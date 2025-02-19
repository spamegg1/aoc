object DataDefs:
  enum Track:
    case Horizontal, Vertical, Intersection, Forwardslash, Backslash
  import Track.*

  type Pos    = (Int, Int)
  type Tracks = Map[Pos, Track]

  val directions = Seq('^', 'v', '>', '<')

  enum Direction:
    case North, South, East, West
    def next(pos: Pos) =
      val (x, y) = pos
      this match
        case North => (x, y - 1)
        case South => (x, y + 1)
        case East  => (x + 1, y)
        case West  => (x - 1, y)
    lazy val reflect = this match // forward slash
      case North => East
      case South => West
      case East  => North
      case West  => South
    lazy val oppose = this match // back slash
      case North => South
      case South => North
      case East  => West
      case West  => East
  import Direction.*

  extension (c: Char)
    def toTrack = c match
      case '-' | '>' | '<' => Horizontal
      case '|' | '^' | 'v' => Vertical
      case '\\'            => Backslash
      case '/'             => Forwardslash
      case '+'             => Intersection
    def toDirection = c match
      case '^' => North
      case 'v' => South
      case '>' => East
      case '<' => West

  enum Rotation:
    case Left, Straight, Right

    def next = this match
      case Left     => Straight
      case Straight => Right
      case Right    => Left

    def intersect(dir: Direction) = (dir, this) match
      case (_, Straight)          => dir
      case (North | South, Right) => dir.reflect
      case (East | West, Left)    => dir.reflect
      case (North | South, Left)  => dir.reflect.oppose
      case (East | West, Right)   => dir.reflect.oppose
  import Rotation.*

  case class Cart(pos: Pos, track: Track, rot: Rotation, dir: Direction):
    lazy val nextDir = track match
      case Horizontal | Vertical => dir
      case Intersection          => rot.intersect(dir)
      case Forwardslash          => dir.reflect
      case Backslash             => dir.reflect.oppose
    lazy val nextPos = nextDir.next(pos)
    lazy val nextRot = track match
      case Intersection => rot.next
      case _            => rot
    def next(using map: Tracks) = Cart(nextPos, map(nextPos), nextRot, nextDir)

  case class Carts(carts: Seq[Cart], cur: Int, total: Int)(using map: Tracks):
    def findCollision = carts.find: c1 =>
      carts.exists(c2 => c1 != c2 && c1.pos == c2.pos)
    def next =
      if cur == total then Carts(carts.sortBy(_.pos), 0, total)
      else
        val newCart  = carts(cur).next
        val newCarts = carts.updated(cur, newCart)
        Carts(newCarts, cur + 1, total)
    def removeCollisions =
      val collisionIndices =
        (for
          i <- 0 until total
          j <- 0 until total
          ci = carts(i)
          cj = carts(j)
          if ci != cj && ci.pos == cj.pos
        yield (i, j)).head
      val (i, j) = collisionIndices // assume i < j
      val remaining = carts.filterNot: c1 => // preserves order
        carts.exists(c2 => c1 != c2 && c1.pos == c2.pos)
      val newCur =
        if cur <= i then cur
        else if cur <= j then cur - 1
        else cur - 2
      Carts(remaining.sortBy(_.pos), newCur, total - 2)

object Parsing:
  import DataDefs.*, Rotation.*

  def parseCartsInLine(line: String, y: Int) = line.zipWithIndex
    .filter(directions contains _._1)
    .map((char, x) => Cart((x, y), char.toTrack, Left, char.toDirection))

  def parseCarts(lines: Seq[String]) = lines.zipWithIndex
    .flatMap(parseCartsInLine)
    .sortBy(_.pos)

  def parseLine(line: String, y: Int) = line.zipWithIndex
    .filterNot(_._1 == ' ')
    .map((char, x) => (x, y) -> char.toTrack)

  def parse(lines: Seq[String]): Tracks = lines.zipWithIndex
    .flatMap(parseLine)
    .toMap

object Solving:
  import DataDefs.*

  def solve1(tracks: Seq[String])(starts: Seq[String]) =
    given Tracks = Parsing.parse(tracks)
    val cartss   = Parsing.parseCarts(starts)
    var carts    = Carts(cartss, 0, cartss.size)
    while !carts.findCollision.isDefined do carts = carts.next
    carts.findCollision.get.pos

  def solve2(tracks: Seq[String])(starts: Seq[String]) =
    given Tracks = Parsing.parse(tracks)
    val cartss   = Parsing.parseCarts(starts)
    var carts    = Carts(cartss, 0, cartss.size)
    while carts.total > 1 do
      if !carts.findCollision.isDefined then carts = carts.next
      else carts = carts.removeCollisions
    carts.carts.head.pos

// I have 2 copies of the input files. One with carts removed.
object Test:
  lazy val file1   = os.pwd / "2018" / "13" / "13.test.input.1.txt"
  lazy val file2   = os.pwd / "2018" / "13" / "13.test.input.2.txt"
  lazy val file3   = os.pwd / "2018" / "13" / "13.test.input.3.txt"
  lazy val file4   = os.pwd / "2018" / "13" / "13.test.input.4.txt"
  lazy val tracks1 = os.read.lines(file1)
  lazy val starts1 = os.read.lines(file2)
  lazy val tracks2 = os.read.lines(file3)
  lazy val starts2 = os.read.lines(file4)
  lazy val res1    = Solving.solve1(tracks1)(starts1)
  lazy val res2    = Solving.solve2(tracks2)(starts2)
// Test.res1 // part 1: 7,3
// Test.res2 // part 2: 6,4

object Main:
  lazy val file1  = os.pwd / "2018" / "13" / "13.input.1.txt"
  lazy val file2  = os.pwd / "2018" / "13" / "13.input.2.txt"
  lazy val tracks = os.read.lines(file1)
  lazy val starts = os.read.lines(file2)
  lazy val res1   = Solving.solve1(tracks)(starts)
  lazy val res2   = Solving.solve2(tracks)(starts)
// Main.res1 // part 1: 118,112
// Main.res2 // part 2: 50,22 this is off-by-1, so it's 50,21
