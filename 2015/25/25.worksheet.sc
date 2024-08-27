/*
--- Day 25: Let It Snow ---
Merry Christmas! Santa is booting up his weather machine;
looks like you might get a white Christmas after all.

The weather machine beeps! On the console of the machine is
a copy protection message asking you to enter a code from
the instruction manual. Apparently, it refuses to run unless
you give it that code. No problem; you'll just look up the code in the--

"Ho ho ho", Santa ponders aloud. "I can't seem to find the manual."

You look up the support number for the manufacturer and give them a call.
Good thing, too - that 49th star wasn't going to earn itself.

"Oh, that machine is quite old!", they tell you.
"That model went out of support six minutes ago,
and we just finished shredding all of the manuals.
I bet we can find you the code generation algorithm, though."

After putting you on hold for twenty minutes
(your call is very important to them, it reminded you repeatedly),
they finally find an engineer that remembers how the code system works.

The codes are printed on an infinite sheet of paper, starting in the top-left corner.
The codes are filled in by diagonals:
starting with the first row with an empty first box,
the codes are filled in diagonally up and to the right.
This process repeats until the infinite paper is covered.
So, the first few codes are filled in in this order:

   | 1   2   3   4   5   6
---+---+---+---+---+---+---+
 1 |  1   3   6  10  15  21
 2 |  2   5   9  14  20
 3 |  4   8  13  19
 4 |  7  12  18
 5 | 11  17
 6 | 16

For example, the 12th code would be written to row 4, column 2;
the 15th code would be written to row 1, column 5.

The voice on the other end of the phone continues with how the
codes are actually generated. The first code is 20151125.
After that, each code is generated by taking the previous one,
multiplying it by 252533, and then keeping the remainder from
dividing that value by 33554393.

So, to find the second code (which ends up in row 2, column 1),
start with the previous value, 20151125. Multiply it by 252533
to get 5088824049625. Then, divide that by 33554393,
which leaves a remainder of 31916031.
That remainder is the second code.

"Oh!", says the voice. "It looks like we missed a scrap from one of the manuals.
Let me read it to you." You write down his numbers:

   |    1         2         3         4         5         6
---+---------+---------+---------+---------+---------+---------+
 1 | 20151125  18749137  17289845  30943339  10071777  33511524
 2 | 31916031  21629792  16929656   7726640  15514188   4041754
 3 | 16080970   8057251   1601130   7981243  11661866  16474243
 4 | 24592653  32451966  21345942   9380097  10600672  31527494
 5 |    77061  17552253  28094349   6899651   9250759  31663883
 6 | 33071741   6796745  25397450  24659492   1534922  27995004

"Now remember", the voice continues, "that's not even all of the first few numbers;
for example, you're missing the one at 7,1 that would come before 6,2.
But, it should be enough to let your-- oh, it's time for lunch! Bye!"
The call disconnects.

Santa looks nervous.
Your puzzle input contains the message on the machine's console.
What code do you give the machine?

--- Part Two ---
The machine springs to life, then falls silent again. It beeps.
"Insufficient fuel", the console reads.
"Fifty stars are required before proceeding. One star is available."

..."one star is available"? You check the fuel tank;
sure enough, a lone star sits at the bottom, awaiting its friends.
Looks like you need to provide 49 yourself.

 */
object Solving:
  def bij(row: Long, col: Long): Long =
    val diagonal = row + col - 1
    val endOfDiagonal = diagonal * (diagonal + 1) / 2 // value at (1, diagonal)
    endOfDiagonal - (diagonal - col)

  private val firstCode = 20151125L
  private val factor = 252533L
  private val divisor = 33554393L
  private val codes = collection.mutable.Map(1L -> firstCode)

  private def generate(target: Long): Unit =
    for index <- 2L to target do
      codes.get(index) match
        case None =>
          val prev = codes(index - 1L)
          val next = (prev * factor) % divisor
          codes(index) = next
        case Some(value) => ()

  def solve1(row: Long, col: Long) =
    val target = bij(row, col)
    generate(target)
    codes.get(target)

object Testing:
  import Solving.*
  assert(bij(1, 1) == 1 && bij(2, 1) == 2 && bij(1, 2) == 3)
  assert(bij(3, 1) == 4 && bij(2, 2) == 5 && bij(1, 3) == 6)
  assert(bij(6, 1) == 16 && bij(4, 3) == 18 && bij(1, 6) == 21)
  lazy val result1 = solve1(6L, 6L)
// Testing.result1 // part 1: 27995004

object Main:
  lazy val result1 = Solving.solve1(2947L, 3029L)
// Main.result1 // part 1: 19980801
