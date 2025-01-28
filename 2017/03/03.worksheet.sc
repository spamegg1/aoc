/*
--- Day 3: Spiral Memory ---
You come across an experimental new kind of memory stored
on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1
and then counting up while spiraling outward. For example, the first few squares are
allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be
carried back to square 1 (the location of the only access port for this memory system) by
programs that can only move up, down, left, or right. They always take the shortest path:
the Manhattan Distance between the location of the data and square 1.

For example:
  Data from square 1 is carried 0 steps, since it's at the access port.
  Data from square 12 is carried 3 steps, such as: down, left, left.
  Data from square 23 is carried only 2 steps: up twice.
  Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle
input all the way to the access port?

Your puzzle input is 361527.

--- Part Two ---
As a stress test on the system, the programs here clear the grid and then store the value
1 in square 1. Then, in the same allocation order as shown above, they store the sum of
the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:
  Square 1 starts with the value 1.
  Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
  Square 3 has both of the above squares as neighbors, stores the sum of their values, 2.
  Square 4 has all three of the aforementioned squares as neighbors and stores the sum of
    their values, 4.
  Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

Once a square is written, its value does not change. Therefore, the first few squares
would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
 */
object DataDefs:
  case class Spiral(number: Int):
    lazy val segment = math.ceil((math.sqrt(number) - 1) / 2.0).toInt
    lazy val offset = number - math.pow(2 * segment - 1, 2).toInt
    lazy val position =
      if 1 <= offset && offset <= 2 * segment then (segment, -segment + offset)
      else if 2 * segment <= offset && offset <= 4 * segment then
        (3 * segment - offset, segment)
      else if 4 * segment <= offset && offset <= 6 * segment then
        (-segment, 5 * segment - offset)
      else (-7 * segment + offset, -segment)
    lazy val distance = math.abs(position._1) + math.abs(position._2)

  case class Position(x: Int, y: Int):
    lazy val next: Position =
      if y < x then // bottom edge or right edge, excluding top-right bottom-left corners
        if math.abs(x) <= math.abs(y) then Position(x + 1, y) // bottom edge incl. bot-rgh
        else Position(x, y + 1) // right edge
      else // top edge or left edge
      if math.abs(x) < math.abs(y) then Position(x - 1, y) // top edge excl. top-left c.
      else if x == y then
        if 0 < x then Position(x - 1, y) // top-right corner
        else Position(x + 1, y) // bottom-left corner
      else Position(x, y - 1) // left edge incl. top-left corner

    lazy val neighbors = Seq(
      Position(x - 1, y + 1),
      Position(x, y + 1),
      Position(x + 1, y + 1),
      Position(x - 1, y),
      Position(x + 1, y),
      Position(x - 1, y - 1),
      Position(x, y - 1),
      Position(x + 1, y - 1)
    )

object Solving:
  import DataDefs.*

  def solve1(input: Int): Int = Spiral(input).distance

  def solve2(input: Int): Int =
    var value = 1
    var pos = Position(0, 0)
    val map = collection.mutable.Map[Position, Int](pos -> value)

    while value <= input do
      pos = pos.next
      value = pos.neighbors.map(neighbor => map.getOrElse(neighbor, 0)).sum
      map(pos) = value

    value

object Test:
  lazy val res1 = Solving.solve1(21)
  lazy val res2 = Solving.solve2(747)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 806

object Main:
  private lazy val input = 361527
  lazy val res1 = Solving.solve1(input)
  lazy val res2 = Solving.solve2(input)
// Main.res1 // part 1: 326
// Main.res2 // part 2: 363010
