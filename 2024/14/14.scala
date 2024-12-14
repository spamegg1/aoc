/*
--- Day 14: Restroom Redoubt ---
One of The Historians needs to use the bathroom;
fortunately, you know there's a bathroom near an unvisited location on their list,
and so you're all quickly teleported directly to the lobby of Easter Bunny Headquarters.

Unfortunately, EBHQ seems to have "improved" bathroom security again after
your last visit. The area outside the bathroom is swarming with robots!

To get The Historian safely to the bathroom, you'll need a way to predict
where the robots will be in the future. Fortunately, they all seem to be
moving on the tile floor in predictable straight lines.

You make a list (your puzzle input) of all of the robots' current positions
(p) and velocities (v), one robot per line. For example:

p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3

Each robot's position is given as p=x,y where x represents the number
of tiles the robot is from the left wall and y represents the number
of tiles from the top wall (when viewed from above).
So, a position of p=0,0 means the robot is all the way in the top-left corner.

Each robot's velocity is given as v=x,y where x and y are given in tiles per second.
Positive x means the robot is moving to the right, and
positive y means the robot is moving down.
So, a velocity of v=1,-2 means that each second,
the robot moves 1 tile to the right and 2 tiles up.

The robots outside the actual bathroom are in a space which is
101 tiles wide and 103 tiles tall (when viewed from above).
However, in this example, the robots are in a space which is only
11 tiles wide and 7 tiles tall.

The robots are good at navigating over/under each other
(due to a combination of springs, extendable legs, and quadcopters),
so they can share the same tile and don't interact with each other.
Visually, the number of robots on each tile in this example looks like this:

1.12.......
...........
...........
......11.11
1.1........
.........1.
.......1...

These robots have a unique feature for maximum bathroom security: they can teleport.
When a robot would run into an edge of the space they're in,
they instead teleport to the other side, effectively wrapping around the edges.
Here is what robot p=2,4 v=2,-3 does for the first few seconds:

Initial state:
...........
...........
...........
...........
..1........
...........
...........

After 1 second:
...........
....1......
...........
...........
...........
...........
...........

After 2 seconds:
...........
...........
...........
...........
...........
......1....
...........

After 3 seconds:
...........
...........
........1..
...........
...........
...........
...........

After 4 seconds:
...........
...........
...........
...........
...........
...........
..........1

After 5 seconds:
...........
...........
...........
.1.........
...........
...........
...........

The Historian can't wait much longer,
so you don't have to simulate the robots for very long.
Where will the robots be after 100 seconds?

In the above example, the number of robots on each tile
after 100 seconds has elapsed looks like this:

......2..1.
...........
1..........
.11........
.....1.....
...12......
.1....1....

To determine the safest area, count the number of robots in each quadrant
after 100 seconds. Robots that are exactly in the middle
(horizontally or vertically) don't count as being in any quadrant,
so the only relevant robots are:

..... 2..1.
..... .....
1.... .....

..... .....
...12 .....
.1... 1....

In this example, the quadrants contain 1, 3, 4, and 1 robot.
Multiplying these together gives a total safety factor of 12.

Predict the motion of the robots in your list within a space
which is 101 tiles wide and 103 tiles tall.
What will the safety factor be after exactly 100 seconds have elapsed?

--- Part Two ---
During the bathroom break, someone notices that these robots seem awfully
similar to ones built and used at the North Pole.
If they're the same type of robots, they should have a hard-coded Easter egg:
very rarely, most of the robots should arrange themselves
into a picture of a Christmas tree.

What is the fewest number of seconds that must elapse
for the robots to display the Easter egg?
 */
package aoc2024.day14

object DataDefs:
  type Size   = (tall: Int, wide: Int)
  type Pos    = (row: Int, col: Int)
  type Vel    = (vert: Int, horz: Int)
  type Robot  = (pos: Pos, vel: Vel)
  type Robots = Seq[Robot]
  type Groups = Seq[Robots]

  enum Dir:
    case N, S, E, W
  import Dir.*

  extension (p: Pos)
    def move(d: Dir): Pos = d match
      case N => (p.row - 1, p.col)
      case S => (p.row + 1, p.col)
      case E => (p.row, p.col + 1)
      case W => (p.row, p.col - 1)
    def neighbors: Seq[Pos] = Dir.values.toSeq.map(p.move)
    def inBounds(tall: Int, wide: Int): Boolean =
      0 <= p.row && p.row < tall && 0 <= p.col && p.col < wide
    def possible(size: Size): Seq[Pos] =
      neighbors.filter(_.inBounds(size.tall, size.wide))

  extension (robot: Robot)
    def next(using size: Size): Robot =
      val (row, col)   = robot.pos
      val (vert, horz) = robot.vel
      val nextRow      = (row + vert + size.tall) % size.tall
      val nextCol      = (col + horz + size.wide) % size.wide
      (pos = (nextRow, nextCol), vel = robot.vel)

  extension (robots: Robots)
    def next(using size: Size): Robots = robots.map(_.next)
    def quadrants(using size: Size) =
      val midTall  = size.tall / 2
      val midWide  = size.wide / 2
      val topLeft  = robots.count(r => r.pos.row < midTall && r.pos.col < midWide)
      val topRight = robots.count(r => r.pos.row < midTall && r.pos.col > midWide)
      val botLeft  = robots.count(r => r.pos.row > midTall && r.pos.col < midWide)
      val botRight = robots.count(r => r.pos.row > midTall && r.pos.col > midWide)
      Seq(topLeft, topRight, botLeft, botRight)

    def groups: Groups = robots.foldLeft(Seq.empty[Robots]): (groups, robot) =>
      val (newGroups, rest) = groups.partition: group =>
        robot.pos.neighbors.exists(p => group.exists(rob => rob.pos == p))
      (Seq(robot) +: newGroups).reduce(_ ++ _) +: rest

    def christmas(groupSize: Int): Boolean = robots.groups.exists(_.size >= groupSize)

    def show(using size: Size): String =
      val blank = Array.fill(size.tall)(Array.fill(size.wide)('.'))
      for
        robot <- robots
        (row, col) = robot.pos
      do blank(row)(col) = '#'
      blank.map(_.mkString).mkString("\n")

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Robot = line match
    case s"p=$col,$row v=$horz,$vert" =>
      ((row.toInt, col.toInt), (vert.toInt, horz.toInt))
  def parse(lines: Seq[String]): Seq[Robot] = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(sec: Int)(using size: Size): Int =
    val startRobots = Parsing.parse(lines)
    val finalRobots = (1 to sec).foldLeft(startRobots)((robots, _) => robots.next)
    finalRobots.quadrants.product

  def solve2(lines: Seq[String])(sec: Int)(using size: Size, groupSize: Int): Unit =
    var robots = Parsing.parse(lines)
    for t <- 1 to sec do
      if t % 100 == 0 then println(s"t: $t secs")
      robots = robots.next
      if robots.christmas(groupSize) then
        println("group found at t = $t secs")
        os.write(os.pwd / "2024" / "14" / s"tree$t", robots.show)

object Testing:
  import DataDefs.Size
  lazy val lines   = os.read.lines(os.pwd / "2024" / "14" / "14.test.input.txt")
  given Size       = (7, 11)
  lazy val result1 = Solving.solve1(lines)(100) // part 1: 12

object Main:
  import DataDefs.Size
  lazy val lines   = os.read.lines(os.pwd / "2024" / "14" / "14.input.txt")
  given Size       = (103, 101)
  given grpSz: Int = 20                          // just try some values here
  lazy val result1 = Solving.solve1(lines)(100)  // part 1: 236628054
  lazy val result2 = Solving.solve2(lines)(8000) // just try some values here

@main
def run: Unit =
  // println(Main.result1)
  Main.result2 // part 2: 7584
