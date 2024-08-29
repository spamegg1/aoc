/*
--- Day 4: Giant Squid ---
You're already almost 1.5km (almost a mile) below the surface of the ocean,
already so deep that you can't see any sunlight. What you can see, however,
is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
Numbers are chosen at random, and the chosen number is marked on all boards
on which it appears. (Numbers may not appear on all boards.) If all numbers
in any row or any column of a board are marked, that board wins.
(Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and
the giant squid) pass the time. It automatically generates a random order
in which to draw numbers and a random set of boards (your puzzle input).
For example:
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners,
but the boards are marked as follows (shown here adjacent to each other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21),
there are still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

At this point, the third board wins because it has at least one complete row
or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum
of all unmarked numbers on that board; in this case, the sum is 188.
Then, multiply that sum by the number that was just called when the board won,
24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first.
What will your final score be if you choose that board?

--- Part Two ---
On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once,
so rather than waste time counting its arms, the safe thing to do is to figure
out which board will win last and choose that one. That way, no matter which
boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after
13 is eventually called and its middle column is completely marked.
If you were to keep playing until this point, the second board would have a
sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?
 */
object DataDefs:
  case class Number(num: Int, var marked: Boolean = false)
  case class Board(id: Int, lines: Seq[Seq[Number]]):
    def mark(draw: Int): Unit =
      for
        row <- lines
        col <- row
        if col.num == draw
      do col.marked = true
    def rowHasWon(row: Int): Boolean = lines(row).forall(_.marked)
    def colHasWon(col: Int): Boolean = (for row <- lines yield row(col)).forall(_.marked)
    def hasWon: Boolean = (0 until lines.size).exists(n => rowHasWon(n) || colHasWon(n))
    def score(draw: Int): Int = lines.flatten.filter(!_.marked).map(_.num).sum * draw

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): Seq[Number] = line
    .split(" ")
    .filter(_.nonEmpty)
    .map(num => Number(num.toInt))
    .toSeq

  private def parseBoard(id: Int, lines: Seq[String]): Board =
    Board(id, lines.map(parseLine))
  private def parseBoards(id: Int, lines: Seq[String]): Seq[Board] =
    if lines.isEmpty then Seq()
    else parseBoard(id, lines.take(5)) +: parseBoards(id + 1, lines.drop(6))

  def parse(lines: Seq[String]) =
    val draws = lines.head.split(",").map(_.toInt).toSeq
    val boards = parseBoards(1, lines.tail.tail)
    (draws, boards)

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def solve1(lines: Seq[String]) =
    val (draws, boards) = Parsing.parse(lines)
    boundary:
      for draw <- draws do
        boards.map(_.mark(draw))
        if boards.exists(_.hasWon) then break(boards.find(_.hasWon).get.score(draw))

  def solve2(lines: Seq[String]) =
    val (draws, boards) = Parsing.parse(lines)
    var winners = List[(Int, Int)]()
    for draw <- draws do
      boards.map(_.mark(draw))
      boards.foreach: board =>
        if board.hasWon && !winners.exists((id, _) => id == board.id) then
          winners = (board.id, board.score(draw)) :: winners
    winners.head

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2021" / "04" / "04.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 4512
// Testing.result2 // part 2: 1924

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2021" / "04" / "04.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 74320
// Main.result2 // part 2: 17884
