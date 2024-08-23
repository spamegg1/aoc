/*
--- Day 19: An Elephant Named Joseph ---
The Elves contact you over a highly secure emergency channel.
Back at the North Pole, the Elves are busy misunderstanding White Elephant parties.

Each Elf brings a present. They all sit in a circle, numbered starting with position 1.
Then, starting with the first Elf, they take turns stealing all the
presents from the Elf to their left. An Elf with no presents is removed
from the circle and does not take turns.

For example, with five Elves (numbered 1 to 5):
  1
5   2
 4 3                                                              11111
  Elf 1 takes Elf 2's present.                                    20111
  Elf 2 has no presents and is skipped.                           20111
  Elf 3 takes Elf 4's present.                                    20201
  Elf 4 has no presents and is also skipped.                      20201
  Elf 5 takes Elf 1's two presents.                               00203
  Neither Elf 1 nor Elf 2 have any presents, so both are skipped. 00203
  Elf 3 takes Elf 5's three presents.                             00500
So, with five Elves, the Elf that sits starting in position 3 gets all the presents.
With the number of Elves given in your puzzle input, which Elf gets all the presents?

--- Part Two ---
Realizing the folly of their present-exchange rules,
the Elves agree to instead steal presents from the Elf directly across the circle.
If two Elves are across the circle, the one on the left
(from the perspective of the stealer) is stolen from.
The other rules remain unchanged:
  Elves with no presents are removed from the circle entirely, and
  the other elves move in slightly to keep the circle evenly spaced.

For example, with five Elves (again numbered 1 to 5):
  The Elves sit in a circle; Elf 1 goes first:
    1             1  2
  5   2         6      3
   4 3            5  4
  Elves 3 and 4 are across the circle;
  Elf 3's present is stolen, being the one to the left.
  Elf 3 leaves the circle, and the rest of the Elves move in:
    1           1
  5   2  -->  5   2
    4           4
  Elf 2 steals from the Elf directly across the circle, Elf 5:
    1         1
  -   2  -->     2
    4         4
  Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:
    -          2
      2  -->
    4          4
  Finally, Elf 2 steals from Elf 4:
    2
      -->  2
    -

So, with five Elves, the Elf that sits starting in position 2 gets all the presents.

With the number of Elves given in your puzzle input, which Elf now gets all the presents?
 */
object DataDefs:
  case class Circle(
      elves: Map[Int, Int],
      cur: Int,
      next: Int,
      size: Int,
      zeros: Int,
      winner: Int
  ):
    def ones = size - zeros
    def finished = ones == 1
    def move1 = // part 1
      if elves(cur) == 0 then copy(cur = (cur + 1) % size, next = (cur + 2) % size)
      else if elves(next) == 0 then copy(next = (next + 1) % size)
      else
        copy(
          elves = elves.updated(cur, elves(cur) + elves(next)).updated(next, 0),
          cur = (next + 1) % size,
          next = (next + 2) % size,
          zeros = zeros + 1,
          winner = cur
        )

    def move2 = // part 2
      if elves(cur) == 0 then copy(cur = (cur + 1) % size)
      else
        val jump = ones / 2
        var i = 0
        var onesCount = 0
        while onesCount < jump do
          i += 1
          if elves((cur + i) % size) != 0 then onesCount += 1
        copy(
          elves = elves.updated((cur + i) % size, 0),
          cur = (cur + 1) % size,
          zeros = zeros + 1,
          winner = cur
        )

object Solving:
  import DataDefs.*

  def solve1(elves: Int) =
    var circle = Circle((0 until elves).map(x => (x, 1)).toMap, 0, 1, elves, 0, 0)
    while !circle.finished do circle = circle.move1
    circle.winner + 1 // due to zero indexing

  def solve2(elves: Int) =
    var circle = Circle((0 until elves).map(x => (x, 1)).toMap, 0, 1, elves, 0, 0)
    while !circle.finished do circle = circle.move2
    circle.winner + 1 // due to zero indexing

object Testing:
  lazy val result1 = List(5, 6, 9, 10) map Solving.solve1
  lazy val result2 = List(5, 6, 9, 10) map Solving.solve2
// Testing.result1 // part 1: 3, 5, 3, 5
// Testing.result2 // part 2: 2, 3, 9, 1

object Main:
  lazy val result1 = Solving.solve1(3014387)
  lazy val result2 = Solving.solve2(30000)
// Main.result1 // part 1: 1834471
// Main.result2 // part 2:
