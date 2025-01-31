/*
--- Day 1: Inverse Captcha ---
The night before Christmas, one of Santa's Elves calls you in a panic. "The
printer's broken! We can't print the Naughty or Nice List!" By the time you make
it to sub-basement 17, there are only a few minutes until midnight. "We have a
big problem," she says; "there must be almost fifty bugs in this system, but
nothing else can print The List. Stand in this square, quick! There's no time to
explain; if you can convince them to pay you in stars, you'll be able to--" She
pulls a lever and the world goes blurry.

When your eyes can focus again, everything seems a lot more pixelated than
before. She must have sent you inside the computer! You check the system clock:
  25 milliseconds until midnight. With that much time, you should be able to
  collect all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
millisecond in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

You're standing in a room with "digitization quarantine" written in LEDs along
one wall. The only door is locked, but it includes a small interface.
"Restricted Area - Strictly No Digitized Users Allowed."

It goes on to explain that you may only leave by solving a captcha to prove
you're not a human. Apparently, you only get one millisecond to solve the
captcha: too fast for a normal human, but it feels like hours to you.

The captcha requires you to review a sequence of digits (your puzzle input) and
find the sum of all digits that match the next digit in the list. The list is
circular, so the digit after the last digit is the first digit in the list.

For example:

    1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the
    second digit and the third digit (2) matches the fourth digit.
    1111 produces 4 because each digit (all 1) matches the next.
    1234 produces 0 because no digit matches the next.
    91212129 produces 9 because the only digit that matches the next one is the
    last digit, 9.

What is the solution to your captcha?

--- Part Two ---
You notice a progress bar that jumps to 50% completion. Apparently, the door
isn't yet satisfied, but it did emit a star as encouragement. The instructions
change:

Now, instead of considering the next digit, it wants you to consider the digit
halfway around the circular list. That is, if your list contains 10 items, only
include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
Fortunately, your list has an even number of elements.

For example:
1212 produces 6: the list contains 4 items, and all four digits match the digit
2 items ahead.
1221 produces 0, because every comparison is between a 1 and a 2.
123425 produces 4, because both 2s match each other, but no other digit has a
match.
123123 produces 12.
12131415 produces 4.

What is the solution to your new captcha?
 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(line: String)(size: Int) =
    val (prefix, suffix) = (line.take(size), line.drop(size))
    val shifted = suffix ++ prefix
    line.map(_.asDigit).zip(shifted.map(_.asDigit))

object Solving:
  import DataDefs.*
  def matches(pairs: IndexedSeq[(Int, Int)]) =
    pairs.filter(pair => pair._1 == pair._2).map(_._1)

  def solve(line: String)(size: Int): Int =
    val pairs = Parsing.parse(line)(size)
    matches(pairs).sum

object Test:
  lazy val input1 = "1122"
  lazy val input2 = "1111"
  lazy val input3 = "1234"
  lazy val input4 = "91212129"
  lazy val res1 = Solving.solve(input1)(1)
  lazy val res2 = Solving.solve(input2)(1)
  lazy val res3 = Solving.solve(input3)(1)
  lazy val res4 = Solving.solve(input4)(1)

  lazy val input5 = "1212"
  lazy val input6 = "1221"
  lazy val input7 = "123425"
  lazy val input8 = "123123"
  lazy val input9 = "12131415"
  lazy val res5 = Solving.solve(input5)(input5.size / 2)
  lazy val res6 = Solving.solve(input6)(input6.size / 2)
  lazy val res7 = Solving.solve(input7)(input7.size / 2)
  lazy val res8 = Solving.solve(input8)(input8.size / 2)
  lazy val res9 = Solving.solve(input9)(input9.size / 2)
// Test.res1 // part 1: 3
// Test.res2 // part 1: 4
// Test.res3 // part 1: 0
// Test.res4 // part 1: 9
// Test.res5 // part 2: 6
// Test.res6 // part 2: 0
// Test.res7 // part 2: 4
// Test.res8 // part 2: 12
// Test.res9 // part 2: 4

object Main:
  lazy val line = os.read.lines(os.pwd / "2017" / "01" / "01.input.txt").head
  lazy val res1 = Solving.solve(line)(1)
  lazy val res2 = Solving.solve(line)(line.size / 2)
// Main.res1 // part 1: 1343
// Main.res2 // part 2: 1274
