/*
--- Day 13: Care Package ---
As you ponder the solitude of space and the ever-increasing three-hour
roundtrip for messages between you and Earth, you notice that the Space
Mail Indicator Light is blinking. To help keep you sane, the Elves have
sent you a care package.

It's a new game for the ship's arcade cabinet! Unfortunately,
the arcade is all the way on the other end of the ship.
Surely, it won't be hard to build your own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent (your puzzle input).
It has a primitive screen capable of drawing square tiles on a grid.
The software draws tiles to the screen with output instructions:
every three output instructions specify the x position (distance from the left),
y position (distance from the top), and tile id. The tile id is interpreted as follows:
  0 is an empty tile. No game object appears in this tile.
  1 is a wall tile. Walls are indestructible barriers.
  2 is a block tile. Blocks can be broken by the ball.
  3 is a horizontal paddle tile. The paddle is indestructible.
  4 is a ball tile. The ball moves diagonally and bounces off objects.

For example, a sequence of output values like 1,2,3,6,5,4 would draw a
horizontal paddle tile (1 tile from the left and 2 tiles from the top)
and a ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game exits?

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parseLine(line: String) = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(line: String) = 0L
  def solve2(line: String) = 0L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2019" / "13" / "13.test.input.txt")
  lazy val result1 = lines map Solving.solve1
  lazy val result2 = lines map Solving.solve2
// Testing.result1 // part 1:
// Testing.result2 // part 2:

object Main:
  lazy val line = os.read.lines(os.pwd / "2019" / "13" / "13.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part 1:
// Main.result2 // part 2:
