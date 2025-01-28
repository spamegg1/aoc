/*
--- Day 9: Marble Mania ---
You talk to the Elves while you wait for your navigation system to initialize.
To pass the time, they introduce you to their favorite marble game.

The Elves play this game by taking turns arranging the marbles in a circle
according to very particular rules. The marbles are numbered starting with
0 and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle.
At this point, while it contains only a single marble,
it is still a circle: the marble is both clockwise from
itself and counter-clockwise from itself.
This marble is designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining
marble into the circle between the marbles that are 1 and 2 marbles
clockwise of the current marble. (When the circle is large enough,
this means that there is one marble between the marble that was just
placed and the current marble.) The marble that was just placed then
becomes the current marble.

However, if the marble that is about to be placed has a number which
is a multiple of 23, something entirely different happens.
First, the current player keeps the marble they would have placed,
adding it to their score. In addition, the marble 7 marbles counter-clockwise
from the current marble is removed from the circle and also added to the current
player's score. The marble located immediately clockwise of the marble that
was removed becomes the new current marble.

For example, suppose there are 9 players. After the marble with value 0 is
placed in the middle, each player (shown in square brackets) takes a turn.
The res of each of those turns would produce circles of marbles like this,
where clockwise is to the right and the resing current marble is in parentheses:

[-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is used up.
Assuming the example above ends after the marble numbered 25,
the winning score is 23+9=32 (because player 5 kept marble 23 and removed marble 9,
while no other player got any points in this very short example game).

Here are a few more examples:
  10 players; last marble is worth 1618 points: high score is 8317
  13 players; last marble is worth 7999 points: high score is 146373
  17 players; last marble is worth 1104 points: high score is 2764
  21 players; last marble is worth 6111 points: high score is 54718
  30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?

--- Part Two ---
Amused by the speed of your answer, the Elves are curious:
What would the new winning Elf's score be if the number
of the last marble were 100 times larger?
 */
import collection.mutable.ArrayDeque

object DataDefs:
  type Scores = Map[Int, Long] // elf player -> that elf's score

  extension (deque: ArrayDeque[Long])
    def shiftLeft(by: Int): Unit = // remove from left-end, append to right-end
      for _ <- 1 to by do // assume by > 0
        val first = deque.removeHead()
        deque.append(first)
    def shiftRight(by: Int): Unit = // remove from right-end, prepend to left-end
      for _ <- 1 to by do // assume by > 0
        val last = deque.removeLast()
        deque.prepend(last)
    def shift(by: Int): Unit = if by < 0 then shiftLeft(-by) else shiftRight(by)

  case class State(
      scores: Scores,
      marbles: ArrayDeque[Long],
      curElf: Int,
      players: Int,
      nextMarble: Long
  ):
    def highScore = scores.values.max // part 1

    def handleMarbles: Long = // mutates marbles, returns score
      if nextMarble % 23L == 0 then
        marbles.shift(7)
        val deleted = marbles.removeHead()
        deleted + nextMarble
      else
        marbles.shift(-2)
        marbles.prepend(nextMarble)
        0L

    def next =
      val score = handleMarbles
      State(
        scores = scores.updated(curElf, scores(curElf) + score),
        marbles = marbles,
        curElf = (curElf + 1) % players,
        players = players,
        nextMarble = nextMarble + 1L
      )

  object State:
    def apply(players: Int) = new State(
      scores = Map.from((0 until players).map(p => (p, 0L))),
      marbles = ArrayDeque(0L),
      curElf = 0,
      players = players,
      nextMarble = 1L
    )

object Solving:
  import DataDefs.*

  def solve(players: Int)(lastMarble: Long) =
    var state = State(players)
    while state.nextMarble <= lastMarble do state = state.next
    state.highScore

object Test:
  val pt1 = Seq((9, 25L), (10, 1618L), (13, 7999L), (17, 1104L), (21, 6111L), (30, 5807L))
  lazy val res1 = pt1.map((p, m) => Solving.solve(p)(m))
// Test.res1 // part 1: 32,8317,146373,2764,54718,37305

object Main:
  lazy val res1 = Solving.solve(470)(72170L)
  lazy val res2 = Solving.solve(470)(7217000L)
// Main.res1 // part 1: 388024
// Main.res2 // part 2: 3180929875
