/*
--- Day 21: Scrambled Letters and Hash ---
The computer system you're breaking into uses a weird scrambling function
to store its passwords. It shouldn't be much trouble to create your own
scrambled password so you can add it to the system;
you just have to implement the scrambler.

The scrambling function is a series of operations
(the exact list is provided in your puzzle input).
Starting with the password to be scrambled,
apply each operation in succession to the string.
The individual operations behave as follows:
  swap position X with position Y means that the letters at indexes
    X and Y (counting from 0) should be swapped.
  swap letter X with letter Y means that the letters
    X and Y should be swapped (regardless of where they appear in the string).
  rotate left/right X steps means that the whole string should be rotated;
    for example, one right rotation would turn abcd into dabc.
  rotate based on position of letter X means that the whole string
    should be rotated to the right based on the index of letter X
    (counting from 0) as determined before this instruction does any rotations.
    Once the index is determined, rotate the string to the right one time,
    plus a number of times equal to that index,
    plus one additional time if the index was at least 4.
  reverse positions X through Y means that the span of letters at indexes
    X through Y (including the letters at X and Y) should be reversed in order.
  move position X to position Y means that the letter which is at index
    X should be removed from the string, then inserted such that it ends up at index Y.

For example, suppose you start with abcde and perform the following operations:
  swap position 4 with position 0 swaps the first and last letters,
    producing the input for the next step, ebcda.
  swap letter d with letter b swaps the positions of d and b: edcba.
  reverse positions 0 through 4 causes the entire string to be reversed, producing abcde.
  rotate left 1 step shifts all letters left one position,
    causing the first letter to wrap to the end of the string: bcdea.
  move position 1 to position 4 removes the letter at position 1 (c),
    then inserts it at position 4 (the end of the string): bdeac.
  move position 3 to position 0 removes the letter at position 3 (a),
    then inserts it at position 0 (the front of the string): abdec.
  rotate based on position of letter b finds the index of letter b (1),
    then rotates the string right once plus
    a number of times equal to that index (2): ecabd.
  rotate based on position of letter d finds the index of letter d (4),
    then rotates the string right once, plus a number of times
    equal to that index, plus an additional time because the
    index was at least 4, for a total of 6 right rotations: decab.

After these steps, the resulting scrambled password is decab.

Now, you just need to generate a new scrambled password and you can access the system.
Given the list of scrambling operations in your puzzle input,
what is the result of scrambling abcdefgh?

--- Part Two ---
You scrambled the password correctly, but you discover that you can't
actually modify the password file on the system. You'll need to un-scramble
one of the existing passwords by reversing the scrambling process.
What is the un-scrambled version of the scrambled password fbgdceah?
 */
object DataDefs:
  enum Op:
    case SwapPos(pos1: Int, pos2: Int)
    case SwapLet(let1: Char, let2: Char)
    case RotLeft(steps: Int)
    case RotRight(steps: Int)
    case RotBasedLeft(letter: Char)
    case RotBasedRight(letter: Char)
    case Reverse(pos1: Int, pos2: Int)
    case Move(pos1: Int, pos2: Int)

    def reverse = this match
      case SwapPos(pos1, pos2)   => this
      case SwapLet(let1, let2)   => this
      case RotLeft(steps)        => RotRight(steps)
      case RotRight(steps)       => RotLeft(steps)
      case RotBasedLeft(letter)  => RotBasedRight(letter)
      case RotBasedRight(letter) => RotBasedLeft(letter)
      case Reverse(pos1, pos2)   => this
      case Move(pos1, pos2)      => Move(pos2, pos1)

  import Op.*

  extension (s: String)
    def operate(op: Op): String = op match
      case SwapPos(pos1, pos2) =>
        s.zipWithIndex
          .map: (char, index) =>
            if index == pos1 then s(pos2)
            else if index == pos2 then s(pos1)
            else char
          .mkString
      case SwapLet(let1, let2) =>
        val (pos1, pos2) = (s.indexWhere(_ == let1), s.indexWhere(_ == let2))
        s.operate(SwapPos(pos1, pos2))
      case RotLeft(steps) =>
        s.indices
          .map(index => s((index + steps) % s.length))
          .mkString
      case RotRight(steps) => s.operate(RotLeft(s.length - steps))
      case RotBasedLeft(letter) =>
        val index = s.indexWhere(_ == letter)
        val steps = 1 + index + (if index >= 4 then 1 else 0)
        s.operate(RotLeft(steps % s.length))
      case RotBasedRight(letter) =>
        val index = s.indexWhere(_ == letter)
        val steps = 1 + index + (if index >= 4 then 1 else 0)
        s.operate(RotRight(steps % s.length))
      case Reverse(pos1, pos2) => // assume pos1 < pos2
        s.take(pos1) + s.drop(pos1).take(pos2 - pos1 + 1).reverse + s.drop(pos2 + 1)
      case Move(pos1, pos2) =>
        if pos1 < pos2 then
          val (head, tail) = (s.take(pos1), s.drop(pos2 + 1))
          val mid = s.drop(pos1 + 1).take(pos2 - pos1)
          head + mid + s"${s(pos1)}" + tail
        else // pos2 < pos1
          val (head, tail) = (s.take(pos2), s.drop(pos1 + 1))
          val mid = s.drop(pos2).take(pos1 - pos2)
          head + s"${s(pos1)}" + mid + tail

object Parsing:
  import DataDefs.*, Op.*

  def parseLine(line: String) = line match
    case s"swap position $pos1 with position $pos2" => SwapPos(pos1.toInt, pos2.toInt)
    case s"swap letter $let1 with letter $let2"     => SwapLet(let1.head, let2.head)
    case s"rotate left $steps steps"                => RotLeft(steps.toInt)
    case s"rotate right $steps steps"               => RotRight(steps.toInt)
    case s"rotate based on position of letter $let" => RotBasedLeft(let.head)
    case s"reverse positions $pos1 through $pos2"   => Reverse(pos1.toInt, pos2.toInt)
    case s"move position $pos1 to position $pos2"   => Move(pos1.toInt, pos2.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(pass: String) = Parsing
    .parse(lines)
    .foldLeft(pass)((scramble, op) => scramble.operate(op))

  def solve2(lines: Seq[String])(pass: String) = Parsing
    .parse(lines)
    .reverse
    .map(_.reverse)
    .foldLeft(pass)((scramble, op) => scramble.operate(op))

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "21.test.input.txt")
  // abcde,ebcda,edcba,abcde,bcdea,bdeac,abdec,ecabd,decab
  lazy val result1 = Solving.solve1(lines)("abcde")
  lazy val result2 = Solving.solve2(lines)("decab")
// Testing.result1 // part 1: decab
Testing.result2 // part 2: abcde

object Main:
  private lazy val lines = os.read.lines(os.pwd / "21.input.txt")
  lazy val result1 = Solving.solve1(lines)("abcdefgh")
  lazy val result2 = Solving.solve2(lines)("fbgdceah")
// Main.result1 // part 1: gcedfahb
// Main.result2 // part 2: efacbdhg
