/*
--- Day 18: Like a GIF For Your Yard ---
After the million lights incident, the fire code has gotten stricter:
now, at most ten thousand lights are allowed. You arrange them in a 100x100 grid.

Never one to let you down, Santa again mails you instructions on
the ideal lighting configuration. With so few lights, he says,
you'll have to resort to animation.

Start by setting your lights to the included initial configuration
(your puzzle input). A # means "on", and a . means "off".

Then, animate your grid in steps, where each step decides the next
configuration based on the current one. Each light's next state
(either on or off) depends on its current state and the current states
of the eight lights adjacent to it (including diagonals).
Lights on the edge of the grid might have fewer than eight neighbors;
the missing ones always count as "off".

For example, in a simplified 6x6 grid,
the light marked A has the neighbors numbered 1 through 8,
and the light marked B, which is on an edge,
only has the neighbors marked 1 through 5:

1B5...
234...
......
..123.
..8A4.
..765.

The state a light should have next is based on its current state
(on or off) plus the number of neighbors that are on:
  A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
  A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.

All of the lights update simultaneously; they all consider
the same current state before moving to the next.

Here's a few steps from an example configuration of another 6x6 grid:

Initial state:
.#.#.#
...##.
#....#
..#...
#.#..#
####..

After 1 step:
..##..
..##.#
...##.
......
#.....
#.##..

After 2 steps:
..###.
......
..###.
......
.#....
.#....

After 3 steps:
...#..
......
...#..
..##..
......
......

After 4 steps:
......
......
..##..
..##..
......
......

After 4 steps, this example has four lights on.

In your grid of 100x100 lights, given your initial configuration,
how many lights are on after 100 steps?

--- Part Two ---
You flip the instructions over;
Santa goes on to point out that this is all just an implementation of
Conway's Game of Life. At least, it was, until you notice that
something's wrong with the grid of lights you bought:
four lights, one in each corner, are stuck on and can't be turned off.
The example above will actually run like this:

Initial state:
##.#.#
...##.
#....#
..#...
#.#..#
####.#

After 1 step:
#.##.#
####.#
...##.
......
#...#.
#.####

After 2 steps:
#..#.#
#....#
.#.##.
...##.
.#..##
##.###

After 3 steps:
#...##
####.#
..##.#
......
##....
####.#

After 4 steps:
#.####
#....#
...#..
.##...
#.....
#.#..#

After 5 steps:
##.###
.##..#
.##...
.##...
#.#...
##...#

After 5 steps, this example now has 17 lights on.

In your grid of 100x100 lights, given your initial configuration,
but with the four corners always in the on state,
how many lights are on after 100 steps?
 */
object DataDefs:
  enum Light:
    case On, Off

    def next(neighbors: Seq[Light]) =
      val on = neighbors.count(_ == On)
      this match
        case On  => if on == 2 || on == 3 then On else Off
        case Off => if on == 3 then On else Off
  import Light.*

  extension (c: Char)
    def toLight = c match
      case '#' => On
      case '.' => Off

  extension (pos: (Int, Int))
    def neighbors =
      val (r, c) = pos
      Seq(
        (r - 1, c - 1),
        (r - 1, c),
        (r - 1, c + 1),
        (r, c - 1),
        (r, c + 1),
        (r + 1, c - 1),
        (r + 1, c),
        (r + 1, c + 1)
      )

    def clamp(size: Int) =
      val (r, c) = pos
      0 <= r && r < size && 0 <= c && c < size

    def isCorner(size: Int) =
      Seq((0, 0), (0, size - 1), (size - 1, 0), (size - 1, size - 1)) contains pos

  case class Grid(lights: Seq[Seq[Light]], size: Int):
    def neighborsOf(row: Int, col: Int) = (row, col).neighbors
      .filter(_.clamp(size))
      .map((r, c) => lights(r)(c))

    lazy val nextLights =
      for row <- 0 until size
      yield for col <- 0 until size
      yield lights(row)(col).next(neighborsOf(row, col))

    lazy val nextLights2 =
      for row <- 0 until size
      yield for
        col <- 0 until size
        pos = (row, col)
      yield
        if pos.isCorner(size) then On
        else lights(row)(col).next(neighborsOf(row, col))

    lazy val next = Grid(nextLights, size)
    lazy val next2 = Grid(nextLights2, size)
    lazy val howManyOn = lights.foldLeft(0)((total, row) => total + row.count(_ == On))

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Seq[Light] = line.map(_.toLight)
  def parseGrid(lines: Seq[String]) = Grid(lines map parseLine, lines.size)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(steps: Int) =
    var start = Parsing.parseGrid(lines)
    for _ <- 0 until steps do start = start.next
    start.howManyOn

  def solve2(lines: Seq[String])(steps: Int) =
    var start = Parsing.parseGrid(lines)
    for _ <- 0 until steps do start = start.next2
    start.howManyOn

object Testing:
  private lazy val lines1 = os.read.lines(os.pwd / "18.test.input.txt")
  private lazy val lines2 = os.read.lines(os.pwd / "18.test.input.2.txt")
  lazy val result1 = Solving.solve1(lines1)(4)
  lazy val result2 = Solving.solve2(lines2)(5)
// Testing.result1 // part 1: 4
// Testing.result2 // part 2: 17

object Main:
  private lazy val lines1 = os.read.lines(os.pwd / "18.input.txt")
  private lazy val lines2 = os.read.lines(os.pwd / "18.input.2.txt")
  lazy val result1 = Solving.solve1(lines1)(100)
  lazy val result2 = Solving.solve2(lines2)(100)
// Main.result1 // part 1: 814
// Main.result2 // part 2: 924
