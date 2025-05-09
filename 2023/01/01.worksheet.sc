/* --- Day 1: Trebuchet?! ---
Something is wrong with global snow production, and you've been selected to take
a look. The Elves have even given you a map; on it, they've used stars to mark
the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you
need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not powerful enough")
and where they're even sending you ("the sky") and why your map looks mostly
blank ("you sure ask a lot of questions") and hang on did you just say the sky
("of course, where do you think snow comes from") when you realize that the
Elves are already loading you into a trebuchet ("please hold still, we need to
strap you in").

As they're making the final adjustments, they discover that their calibration
document (your puzzle input) has been amended by a very young Elf who was
apparently just excited to show off her art skills. Consequently, the Elves are
having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line
originally contained a specific calibration value that the Elves now need to
recover. On each line, the calibration value can be found by combining the first
digit and the last digit (in that order) to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet

In this example, the calibration values of these four lines are 12, 38, 15, and
77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of the
calibration values?

--- Part Two ---
Your calculation isn't quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six, seven,
eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last
digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76.
Adding these together produces 281.

What is the sum of all of the calibration values?
 */
object DataDefs:
  val numbers = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9
  )

  val regex = "(?=([1-9]|one|two|three|four|five|six|seven|eight|nine))".r

object Solving:
  import DataDefs.*

  def calibrationValue(line: String): Int = // Part 1
    val nums = line.filter(_.isDigit)
    nums.head.asDigit * 10 + nums.last.asDigit

  def lineToValue(line: String): Int = // Part 2
    val matches = regex.findAllMatchIn(line).map(_.group(1)).toVector
    val (first, last) = (numbers(matches.head), numbers(matches.last))
    first * 10 + last

  def solve1(lines: Seq[String]) = lines.map(calibrationValue).sum
  def solve2(lines: Seq[String]) = lines.map(lineToValue).sum

object Test: // Handle overlapping matches
  val eightyThree = Solving.lineToValue("eighthree") // should be 83 not 88
  val seventyNine = Solving.lineToValue("sevenine") // should be 79 not 77

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2023" / "01" / "01.input.txt")
  val res1 = Solving.solve1(lines)
  val res2 = Solving.solve2(lines)
// Main.res1 // 54708
// Main.res2 // 54087
