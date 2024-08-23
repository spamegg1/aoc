/*
--- Day 4: Secure Container ---
You arrive at the Venus fuel depot only to discover it's protected by a password.
The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:
  It is a six-digit number.
  The value is within the range given in your puzzle input.
  Two adjacent digits are the same (like 22 in 122345).
  Going from left to right, the digits never decrease;
  they only ever increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:
  111111 meets these criteria (double 11, never decreases).
  223450 does not meet these criteria (decreasing pair of digits 50).
  123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input
meet these criteria? Your puzzle input is 156218-652527.

--- Part Two ---
An Elf just remembered one more important detail:
the two adjacent matching digits are not part of a larger group of matching digits.

Given this additional criterion, but still ignoring the range rule,
the following are now true:
  112233 meets these criteria because the digits never decrease and
    all repeated digits are exactly two digits long.
  123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
  111122 meets the criteria
    (even though 1 is repeated more than twice, it still contains a double 22).

How many different passwords within the range given
in your puzzle input meet all of the criteria?
 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) =
    (156218 to 652527).view
      .map(_.toString)
      .filter(str => (0 to 4).forall(index => str(index) <= str(index + 1)))
      .filter(str => (0 to 4).exists(index => str(index) == str(index + 1)))
      .size

  def solve2(lines: Seq[String]) =
    (156218 to 652527).view
      .map(_.toString)
      .filter(str => (0 to 4).forall(index => str(index) <= str(index + 1)))
      .filter(str =>
        (0 to 4).exists(index =>
          str(index) == str(index + 1) && str.count(_ == str(index)) == 2
        )
      )
      .size

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "04.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1:
// Testing.result2 // part 2:

object Main:
  lazy val lines = os.read.lines(os.pwd / "04.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 1694
Main.result2 // part 2: 1148
