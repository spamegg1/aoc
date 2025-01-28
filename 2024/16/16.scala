/*
--- Day 16: Reindeer Maze ---
It's time again for the Reindeer Olympics!
This year, the big event is the Reindeer Maze,
where the Reindeer compete for the lowest score.

You and The Historians arrive to search for the Chief right as the event
is about to start. It wouldn't hurt to watch a little, right?

The Reindeer start on the Start Tile (marked S) facing East and need to
reach the End Tile (marked E). They can move forward one tile at a time
(increasing their score by 1 point), but never into a wall (#).
They can also rotate clockwise or counterclockwise 90 degrees at a time
(increasing their score by 1000 points).

To figure out the best place to sit, you start by grabbing a map (your puzzle input)
from a nearby kiosk. For example:

###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############

There are many paths through this maze, but taking any of the best paths
would incur a score of only 7036. This can be achieved by taking a total
of 36 steps forward and turning 90 degrees a total of 7 times:

###############
#.......#....E#
#.#.###.#.###^#
#.....#.#...#^#
#.###.#####.#^#
#.#.#.......#^#
#.#.#####.###^#
#..>>>>>>>>v#^#
###^#.#####v#^#
#>>^#.....#v#^#
#^#.#.###.#v#^#
#^....#...#v#^#
#^###.#.#.#v#^#
#S..#.....#>>^#
###############

Here's a second example:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################

In this maze, the best paths cost 11048 points;
following one such path would look like this:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#^#
#.#.#.#...#...#^#
#.#.#.#.###.#.#^#
#>>v#.#.#.....#^#
#^#v#.#.#.#####^#
#^#v..#.#.#>>>>^#
#^#v#####.#^###.#
#^#v#..>>>>^#...#
#^#v###^#####.###
#^#v#>>^#.....#.#
#^#v#^#####.###.#
#^#v#^........#.#
#^#v#^#########.#
#S#>>^..........#
#################

Note that the path shown above includes one 90 degree turn as the very first move,
rotating the Reindeer from facing East to facing North.

Analyze your map carefully. What is the lowest score a Reindeer could possibly get?

--- Part Two ---
Now that you know what the best paths look like,
you can figure out the best spot to sit.

Every non-wall tile (S, ., or E) is equipped with places to sit
along the edges of the tile. While determining which of these tiles
would be the best spot to sit depends on a whole bunch of factors
(how comfortable the seats are, how far away the bathrooms are,
whether there's a pillar blocking your view, etc.),
the most important factor is whether the tile is on one of the
best paths through the maze. If you sit somewhere else,
you'd miss all the action!

So, you'll need to determine which tiles are part of any best path
through the maze, including the S and E tiles.

In the first example, there are 45 tiles (marked O)
that are part of at least one of the various best paths through the maze:

###############
#.......#....O#
#.#.###.#.###O#
#.....#.#...#O#
#.###.#####.#O#
#.#.#.......#O#
#.#.#####.###O#
#..OOOOOOOOO#O#
###O#O#####O#O#
#OOO#O....#O#O#
#O#O#O###.#O#O#
#OOOOO#...#O#O#
#O###.#.#.#O#O#
#O..#.....#OOO#
###############

In the second example, there are 64 tiles that are part of
at least one of the best paths:

#################
#...#...#...#..O#
#.#.#.#.#.#.#.#O#
#.#.#.#...#...#O#
#.#.#.#.###.#.#O#
#OOO#.#.#.....#O#
#O#O#.#.#.#####O#
#O#O..#.#.#OOOOO#
#O#O#####.#O###O#
#O#O#..OOOOO#OOO#
#O#O###O#####O###
#O#O#OOO#..OOO#.#
#O#O#O#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#O#OOO..........#
#################

Analyze your map further. How many tiles are part of
at least one of the best paths through the maze?
 */
package aoc2024.day16

object DataDefs:
  val WALL = '#'
  val FREE = '.'

  enum Dir:
    case N, S, E, W
    def score(that: Dir): Long = (this, that) match
      case (N, E) | (N, W) => 1001L
      case (S, E) | (S, W) => 1001L
      case (E, N) | (E, S) => 1001L
      case (W, N) | (W, S) => 1001L
      case _               => 1L
  import Dir.*

  extension (dirs: Seq[Dir]) def score: Long = dirs.zip(dirs.tail).map(_.score(_)).sum

  type Pos   = (row: Int, col: Int)
  type Move  = (dir: Dir, pos: Pos)
  type Rein  = (pos: Pos, visited: Moves)
  type Moves = Seq[Move]
  type Reins = Seq[Rein]
  type Maze  = Seq[String]

  extension (p: Pos)
    def move(dir: Dir): Pos = dir match
      case N => (p.row - 1, p.col)
      case S => (p.row + 1, p.col)
      case E => (p.row, p.col + 1)
      case W => (p.row, p.col - 1)
    def neighbors: Moves = Dir.values.toSeq.map(d => (d, p.move(d)))
    def possible(visited: Moves)(using maze: Maze): Moves =
      p.neighbors.filter: move =>
        !visited.exists(_.pos == move.pos) &&
          maze(move.pos.row)(move.pos.col) != WALL

  extension (r: Rein)
    def next(using maze: Maze): Reins = r.pos
      .possible(r.visited)
      .map(move => (pos = move.pos, visited = move +: r.visited))

  extension (rs: Reins) def next(using maze: Maze): Reins = rs.flatMap(_.next)
  object Reins:
    def apply(start: Pos) = Seq((pos = start, visited = Seq((dir = E, pos = start))))

object Solving:
  import DataDefs.*

  def solve1(start: Pos)(finish: Pos)(using maze: Seq[String]) =
    var reins  = Reins(start)
    var res = Seq.empty[Rein]

    while res.size < 5 do
      println(res.size)
      val (finished, going) = reins.next.partition(_.pos == finish)
      res ++= finished
      reins = going

    res.map(_.visited.map(_.dir)).minBy(_.score).score

  def solve2(maze: Seq[String])(start: Pos)(finish: Pos) = 0L

object Test:
  lazy val lines1   = os.read.lines(os.pwd / "2024" / "16" / "16.test.input.1.txt")
  lazy val lines2   = os.read.lines(os.pwd / "2024" / "16" / "16.test.input.2.txt")
  lazy val res11 = Solving.solve1((13, 1))((1, 13))(using lines1)
  lazy val res12 = Solving.solve1((15, 1))((1, 15))(using lines2)
  lazy val res21 = Solving.solve2(lines1) // 45
  lazy val res22 = Solving.solve2(lines1) // 64

object Main:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "16" / "16.input.txt")
  lazy val res1 = Solving.solve1((139, 1))((1, 139))(using lines)
  lazy val res2 = Solving.solve2(lines)

@main
def run: Unit =
  // println(Test.res11) // part 1: 7036
  // println(Test.res12) // part 1: 11048
  println(Main.res1)     // part 1: 160624
  println(Main.res1)     // part 2: 692
