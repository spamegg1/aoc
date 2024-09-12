/*
--- Day 6: Chronal Coordinates ---
The device on your wrist beeps several times, and once again you feel like you're falling.

"Situation critical," the device announces. "Destination indeterminate.
Chronal interference detected. Please specify new target coordinates."

The device then produces a list of coordinates (your puzzle input).
Are they places it thinks are safe or dangerous?
It recommends you check manual page 729. The Elves did not give you a manual.

If they're dangerous, maybe you can minimize the danger by finding the
coordinate that gives the largest distance from the other points.

Using only the Manhattan distance, determine the area around each
coordinate by counting the number of integer X,Y locations that are
closest to that coordinate (and aren't tied in distance to any other coordinate).

Your goal is to find the size of the largest area that isn't infinite.
For example, consider the following list of coordinates:

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9

If we name these coordinates A through F,
we can draw them on a grid, putting 0,0 at the top left:

..........
.A........
..........
........C.
...D......
.....E....
.B........
..........
..........
........F.

This view is partial - the actual grid extends infinitely in all directions.
Using the Manhattan distance, each location's closest
coordinate can be determined, shown here in lowercase:

aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

Locations shown as . are equally far from two or more coordinates,
and so they don't count as being closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite -
while not shown here, their areas extend forever outside the visible grid.
However, the areas of coordinates D and E are finite:
  D is closest to 9 locations, and E is closest to 17
  (both including the coordinate's location itself).
  Therefore, in this example, the size of the largest area is 17.

What is the size of the largest area that isn't infinite?

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = ???

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "06" / "06.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: ???
// Testing.result2 // part 2: ???

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "06" / "06.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: ???
// Main.result2 // part 2: ???
