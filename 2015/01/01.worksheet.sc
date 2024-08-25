/*
--- Day 1: Not Quite Lisp ---
Santa was hoping for a white Christmas, but his weather machine's "snow"
function is powered by stars, and he's fresh out! To save Christmas, he needs
you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made available
on each day in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't
find the right floor - the directions he got are a little confusing. He starts
on the ground floor (floor 0) and then follows the instructions one character at
a time.

An opening parenthesis, (, means he should go up one floor, and a closing
parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will
never find the top or bottom floors.

For example:
    (()) and ()() both result in floor 0.
    ((( and (()(()( both result in floor 3.
    ))((((( also results in floor 3.
    ()) and ))( both result in floor -1 (the first basement level).
    ))) and )())()) both result in floor -3.
To what floor do the instructions take Santa?

--- Part Two ---
Now, given the same instructions, find the position of the first character that
causes him to enter the basement (floor -1). The first character in the
instructions has position 1, the second character has position 2, and so on.

For example:
    ) causes him to enter the basement at character position 1.
    ()()) causes him to enter the basement at character position 5.
What is the position of the character that causes Santa to first enter the
basement?
 */

object Solving:
  import util.boundary, boundary.break

  def solve1(line: String) = line.foldLeft(0): (acc, c) =>
    acc + (if c == '(' then 1 else -1)

  def solve2(line: String) =
    var index = 0
    var acc = 0
    boundary:
      while 0 <= acc do
        val char = line(index)
        acc += (if char == '(' then 1 else -1)
        if acc < 0 then break()
        else index += 1
    index + 1

object Testing:
  lazy val file = os.pwd / "2015" / "01" / "01.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val result1 = lines map Solving.solve1
// Testing.result1 // part1: 0,0,3,3,3,-1,-1,-3,-3

object Main:
  lazy val file = os.pwd / "2015" / "01" / "01.input.txt"
  lazy val line = os.read(file)
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part1: 74
// Main.result2 // part2: 1795
