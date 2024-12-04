/*
--- Day 4: Ceres Search ---
"Looks like the Chief's not here. Next!"
One of The Historians pulls out a device and pushes the only button on it.
After a brief flash, you recognize the interior of the Ceres monitoring station!

As the search for the Chief continues, a small Elf who lives on the station
tugs on your shirt; she'd like to know if you could help her with her word search
(your puzzle input). She only has to find one word: XMAS.

This word search allows words to be horizontal, vertical, diagonal,
written backwards, or even overlapping other words.
It's a little unusual, though, as you don't merely need to find one instance of XMAS,
you need to find all of them. Here are a few ways XMAS might appear,
where irrelevant characters have been replaced with .:

..X...
.SAMX.
.A..A.
XMAS.S
.X....

The actual word search will be full of letters instead. For example:

MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX

In this word search, XMAS occurs a total of 18 times;
here's the same word search again,
but where letters not involved in any XMAS have been replaced with .:

....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX

Take a look at the little Elf's word search. How many times does XMAS appear?

--- Part Two ---
The Elf looks quizzically at you. Did you misunderstand the assignment?

Looking for the instructions, you flip over the word search to find
that this isn't actually an XMAS puzzle;
it's an X-MAS puzzle in which you're supposed to find two MAS in
the shape of an X. One way to achieve that is like this:

M.S
.A.
M.S

Irrelevant characters have again been replaced with . in the above diagram.
Within the X, each MAS can be written forwards or backwards.

Here's the same example from before,
but this time all of the X-MASes have been kept instead:

.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........

In this example, an X-MAS appears 9 times.

Flip the word search from the instructions back over to
the word search side and try again. How many times does an X-MAS appear?
 */
object DataDefs:
  val xmas = """(?=(XMAS|SAMX))""".r
  val mas = Seq("MAS", "SAM")

  case class Grid(rows: Seq[String], size: Int): // assume grid is square
    lazy val cols: Seq[String] =
      for j <- 0 until size yield (for i <- 0 until size yield rows(i)(j)).mkString
    // there are 4 * size - 2 many diagonals
    lazy val ul: Seq[String] =
      for i <- 0 until size yield (for j <- 0 to i yield rows(i - j)(j)).mkString
    lazy val lr: Seq[String] =
      for j <- 1 until size
      yield (for i <- j until size yield rows(i)(size - i + j - 1)).mkString
    lazy val ll: Seq[String] =
      for i <- 0 until size
      yield (for j <- 0 until size - i yield rows(i + j)(j)).mkString
    lazy val ur: Seq[String] =
      for j <- 1 until size
      yield (for i <- 0 until size - j yield rows(i)(i + j)).mkString
    lazy val diags = ul ++ lr ++ ll ++ ur
    lazy val all = rows ++ cols ++ diags

    def x(i: Int, j: Int) = // part 2
      require(0 < i && i < size && 0 < j && j < size)
      val left = Seq(rows(i - 1)(j - 1), rows(i)(j), rows(i + 1)(j + 1)).mkString
      val right = Seq(rows(i + 1)(j - 1), rows(i)(j), rows(i - 1)(j + 1)).mkString
      (left, right)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    Grid(lines, lines.size).all
      .map(line => xmas.findAllMatchIn(line).size)
      .sum

  def solve2(lines: Seq[String]) =
    val grid = Grid(lines, lines.size)
    (for
      i <- 1 until grid.size - 1
      j <- 1 until grid.size - 1
      (left, right) = grid.x(i, j)
      if mas.contains(left) && mas.contains(right)
    yield 1).sum

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2024" / "04" / "04.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 18
// Testing.result2 // part 2: 9

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2024" / "04" / "04.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 2545
// Main.result2 // part 2: 1886
