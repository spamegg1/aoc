/*
--- Day 6: Guard Gallivant ---
The Historians use their fancy device again, this time to whisk
you all away to the North Pole prototype suit manufacturing lab...
in the year 1518! It turns out that having direct access to history
is very convenient for a group of historians.

You still have to be careful of time paradoxes,
and so it will be important to avoid anyone from 1518
while The Historians search for the Chief.
Unfortunately, a single guard is patrolling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that
The Historians can search safely?

You start by making a map (your puzzle input) of the situation.
For example:

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...

The map shows the current position of the guard with ^
(to indicate the guard is currently facing up from the perspective of the map).
Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

Lab guards in 1518 follow a very strict patrol protocol which
involves repeatedly following these steps:
  If there is something directly in front of you, turn right 90 degrees.
  Otherwise, take a step forward.

Following the above protocol, the guard moves up several times
until she reaches an obstacle (in this case, a pile of failed suit prototypes):

....#.....
....^....#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Because there is now an obstacle in front of the guard,
she turns right before continuing straight in her new facing direction:

....#.....
........>#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Reaching another obstacle (a spool of several very long polymers),
she turns right again and continues downward:

....#.....
.........#
..........
..#.......
.......#..
..........
.#......v.
........#.
#.........
......#...

This process continues for a while, but the guard eventually leaves
the mapped area (after walking past a tank of universal solvent):

....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#v..

By predicting the guard's route, you can determine which specific
positions in the lab will be in the patrol path.
Including the guard's starting position, the positions visited
by the guard before leaving the area are marked with an X:

....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..

In this example, the guard will visit 41 distinct positions on your map.

Predict the path of the guard. How many distinct positions will
the guard visit before leaving the mapped area?

--- Part Two ---
While The Historians begin working around the guard's patrol route,
you borrow their fancy device and step outside the lab.
From the safety of a supply closet, you time travel through the
last few months and record the nightly status of the lab's guard post
on the walls of the closet.

Returning after what seems like only a few seconds to The Historians,
they explain that the guard's patrol area is simply too large for them
to safely search the lab without getting caught.

Fortunately, they are pretty sure that adding a single new obstruction
won't cause a time paradox. They'd like to place the new obstruction
in such a way that the guard will get stuck in a loop,
making the rest of the lab safe to search.

To have the lowest chance of creating a time paradox,
The Historians would like to know all of the possible positions for such an obstruction.
The new obstruction can't be placed at the guard's starting position
 - the guard is there right now and would notice.

In the above example, there are only 6 different positions where
a new obstruction would cause the guard to get stuck in a loop.
The diagrams of these six situations use O to mark the new obstruction,
| to show a position where the guard moves up/down,
- to show a position where the guard moves left/right,
and + to show a position where the guard moves both up/down and left/right.

Option one, put a printing press next to the guard's starting position:

....#.....
....+---+#
....|...|.
..#.|...|.
....|..#|.
....|...|.
.#.O^---+.
........#.
#.........
......#...

Option two, put a stack of failed suit prototypes
in the bottom right quadrant of the mapped area:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
......O.#.
#.........
......#...

Option three, put a crate of chimney-squeeze prototype fabric
next to the standing desk in the bottom right quadrant:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----+O#.
#+----+...
......#...

Option four, put an alchemical retroencabulator near the bottom left corner:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
..|...|.#.
#O+---+...
......#...

Option five, put the alchemical retroencabulator a bit to the right instead:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
....|.|.#.
#..O+-+...
......#...

Option six, put a tank of sovereign glue right next to the tank of universal solvent:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..

It doesn't really matter what you choose to use as an obstacle
so long as you and The Historians can put it into position
without the guard noticing. The important thing is having enough
options that you can find one that minimizes time paradoxes,
and in this example, there are 6 different positions you could choose.

You need to get the guard stuck in a loop by adding a single new obstruction.
How many different positions could you choose for this obstruction?
 */
package aoc202406

import collection.parallel.CollectionConverters.*

object DataDefs:
  enum Dir:
    case N, S, E, W
    def turn = this match
      case N => E
      case S => W
      case E => S
      case W => N
  import Dir.*

  type Pos = (row: Int, col: Int) // named tuples yay!
  extension (p: Pos)
    def inBounds(size: Int) =
      0 <= p.row && p.row < size && 0 <= p.col && p.col < size
    def next(d: Dir): Pos = d match
      case N => (p.row - 1, p.col)
      case S => (p.row + 1, p.col)
      case E => (p.row, p.col + 1)
      case W => (p.row, p.col - 1)

  case class Guard(p: Pos, d: Dir, vis: Map[Pos, Int])(using map: Seq[String], size: Int):
    val nPos              = p.next(d) // do we go out of bounds at the next step?
    def inBounds: Boolean = nPos.inBounds(size)
    def looped      = vis.values.exists(_ > 4) // vis.size > size * size
    def newPos: Pos = if inBounds && map(nPos.row)(nPos.col) == '#' then p else nPos
    def next: Guard =
      if newPos == p then copy(d = d.turn)
      else copy(p = newPos, vis = vis.updated(newPos, vis.getOrElse(newPos, 0) + 1))

object Solving:
  import DataDefs.*

  def generateMaps(map: Seq[String], path: Seq[Pos]) =
    for (row, col) <- path yield map.updated(row, map(row).updated(col, '#'))

  def solve(map: Seq[String])(startPos: Pos, startDir: Dir) =
    var guard = Guard(startPos, startDir, Map(startPos -> 1))(using map, map.size)
    while guard.inBounds && !guard.looped do guard = guard.next
    guard // needed in parts 1, 2

  def solve1(map: Seq[String])(startPos: Pos, startDir: Dir) =
    solve(map)(startPos, startDir).vis.size

  def solve2(map: Seq[String])(startPos: Pos, startDir: Dir) =
    val guard = solve(map)(startPos, startDir) // guard from part 1 finished its path
    val path  = guard.vis.keys.toSeq diff Seq(startPos)
    val maps  = generateMaps(map, path)        // .par
    (for
      m <- maps
      g = solve(m)(startPos, startDir)
      if g.looped
    yield 1).size

object Test: // 6, 4, N
  private lazy val lines = os.read.lines(os.pwd / "2024" / "06" / "06.test.input.txt")
  lazy val res1       = Solving.solve1(lines)((6, 4), DataDefs.Dir.N)
  lazy val res2       = Solving.solve2(lines)((6, 4), DataDefs.Dir.N)
// Test.res1 // part 1: 41
// Test.res2 // part 2: 6

object Main: // 94, 73, N
  private lazy val lines = os.read.lines(os.pwd / "2024" / "06" / "06.input.txt")
  lazy val res1       = Solving.solve1(lines)((94, 73), DataDefs.Dir.N)
  lazy val res2       = Solving.solve2(lines)((94, 73), DataDefs.Dir.N)
// Main.res1 // part 1: 5242
// Main.res2 // part 2: 1424

@main def run = println(Main.res2)
