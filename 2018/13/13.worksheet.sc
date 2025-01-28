/*
--- Day 13: Mine Cart Madness ---
A crop of this size requires significant logistics to transport produce,
soil, fertilizer, and so on. The Elves are very busy pushing things
around in carts on some kind of rudimentary system of tracks they've come up with.

Seeing as how cart-and-track systems don't appear in recorded history
for another 1000 years, the Elves seem to be making this up as they go along.
They haven't even figured out how to avoid collisions yet.

You map out the tracks (your puzzle input) and see where you can help.

Tracks consist of straight paths (| and -),
curves (/ and \), and intersections (+).
Curves connect exactly two perpendicular pieces of track; for example,
this is a closed loop:

/----\
|    |
|    |
\----/

Intersections occur when two perpendicular paths cross. At an intersection,
a cart is capable of turning left, turning right, or continuing straight.
Here are two loops connected by two intersections:

/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/

Several carts are also on the tracks. Carts always face either
up (^), down (v), left (<), or right (>). (On your initial map,
the track under each cart is a straight path
matching the direction the cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection),
it turns left the first time, goes straight the second time,
turns right the third time, and then repeats those directions
starting again with left the fourth time, straight the fifth time,
and so on. This process is independent of the particular
intersection at which the cart has arrived - that is, the cart has
no per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a time.
They do this based on their current location: carts on the top row move first
(acting from left to right), then carts on the second row move
(again from left to right), then carts on the third row, and so on.
Once each cart has moved one step, the process repeats;
each of these loops is called a tick.

For example, suppose there are two carts on a straight track:

|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |

First, the top cart moves. It is facing down (v), so it moves down one square.
Second, the bottom cart moves. It is facing up (^), so it moves up one square.
Because all carts have moved, the first tick ends. Then, the process repeats,
starting with the first cart. The first cart moves down,
then the second cart moves up - right into the first cart, colliding with it!
(The location of the crash is marked with an X.)
This ends the second and last tick.

Here is a longer example:

/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/-->\
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \->--/
  \------/

/---v
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+>-/
  \------/

/---\
|   v  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+->/
  \------/

/---\
|   |  /----\
| /->--+-\  |
| | |  | |  |
\-+-/  \-+--^
  \------/

/---\
|   |  /----\
| /-+>-+-\  |
| | |  | |  ^
\-+-/  \-+--/
  \------/

/---\
|   |  /----\
| /-+->+-\  ^
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /---<\
| /-+--+>\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /--<-\
| /-+--+-v  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /-<--\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/---\
|   |  /<---\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-<--/
  \------/

/---\
|   |  v----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \<+--/
  \------/

/---\
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/

/---\
|   |  /----\
| /-+--+-\  |
| | |  X |  |
\-+-/  \-+--/
  \------/

After following their respective paths for a while, the carts eventually crash.
To help prevent crashes, you'd like to know the location of the first crash.
Locations are given in X,Y coordinates,
where the furthest left column is X=0 and the furthest top row is Y=0:

           111
 0123456789012
0/---\
1|   |  /----\
2| /-+--+-\  |
3| | |  X |  |
4\-+-/  \-+--/
5  \------/

In this example, the location of the first crash is 7,3.
What is the location of the first crash?

--- Part Two ---
There isn't much you can do to prevent crashes in this ridiculous system.
However, by predicting the crashes, the Elves know where to be in advance
and instantly remove the two crashing carts the moment any crash occurs.

They can proceed like this for a while, but eventually,
they're going to run out of carts. It could be useful to
figure out where the last cart that hasn't crashed will end up.

For example:

/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/

/---\
|   |
| v-+-\
| | | |
\-+-/ |
  |   |
  ^---^

/---\
|   |
| /-+-\
| v | |
\-+-/ |
  ^   ^
  \---/

/---\
|   |
| /-+-\
| | | |
\-+-/ ^
  |   |
  \---/

After four very expensive crashes, a tick ends with only
one cart remaining; its final location is 6,4.

What is the location of the last cart at the end of the
first tick where it is the only cart left?
 */
object DataDefs:
  enum Track:
    case Horizontal, Vertical, Intersection, Forwardslash, Backslash
  import Track.*

  type Pos = (Int, Int)
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
        val newCart = carts(cur).next
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
    val cartss = Parsing.parseCarts(starts)
    var carts = Carts(cartss, 0, cartss.size)
    while !carts.findCollision.isDefined do carts = carts.next
    carts.findCollision.get.pos

  def solve2(tracks: Seq[String])(starts: Seq[String]) =
    given Tracks = Parsing.parse(tracks)
    val cartss = Parsing.parseCarts(starts)
    var carts = Carts(cartss, 0, cartss.size)
    while carts.total > 1 do
      if !carts.findCollision.isDefined then carts = carts.next
      else carts = carts.removeCollisions
    carts.carts.head.pos

// I have 2 copies of the input files. One with carts removed.
object Test:
  lazy val tracks1 = os.read.lines(os.pwd / "2018" / "13" / "13.test.input.txt")
  lazy val starts1 = os.read.lines(os.pwd / "2018" / "13" / "13.test.input.2.txt")
  lazy val tracks2 = os.read.lines(os.pwd / "2018" / "13" / "13.test.input.3.txt")
  lazy val starts2 = os.read.lines(os.pwd / "2018" / "13" / "13.test.input.4.txt")
  lazy val res1 = Solving.solve1(tracks1)(starts1)
  lazy val res2 = Solving.solve2(tracks2)(starts2)
// Test.res1 // part 1: 7,3
// Test.res2 // part 2: 6,4

object Main:
  lazy val tracks = os.read.lines(os.pwd / "2018" / "13" / "13.input.txt")
  lazy val starts = os.read.lines(os.pwd / "2018" / "13" / "13.input.2.txt")
  lazy val res1 = Solving.solve1(tracks)(starts)
  lazy val res2 = Solving.solve2(tracks)(starts)
// Main.res1 // part 1: 118,112
// Main.res2 // part 2: 50,22 this is off-by-1, so it's 50,21
