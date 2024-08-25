/*
--- Day 12: JSAbacusFramework.io ---
Santa's Accounting-Elves need help balancing the books after a recent order.
Unfortunately, their accounting software uses a peculiar storage format.
That's where you come in.

They have a JSON document which contains a variety of things:
  arrays ([1,2,3]),
  objects ({"a":1, "b":2}),
  numbers, and
  strings.
Your first job is to simply find all of the numbers
throughout the document and add them together.

For example:
  [1,2,3] and {"a":2,"b":4} both have a sum of 6.
  [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
  {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
  [] and {} both have a sum of 0.

You will not encounter any strings containing numbers.
What is the sum of all numbers in the document?

--- Part Two ---
Uh oh - the Accounting-Elves have realized that they double-counted everything red.

Ignore any object (and all of its children) which has any property with the value "red".
Do this only for objects ({...}), not arrays ([...]).
  [1,2,3] still has a sum of 6.
  [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
  {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0,
    because the entire structure is ignored.
  [1,"red",5] has a sum of 6, because "red" in an array has no effect.
 */
import ujson.*

object Solving:
  def findNumbers(pred: Value => Boolean)(json: Value): Double = json match
    case Obj(value) =>
      if pred(value) then 0.0
      else value.values.map(findNumbers(pred)).sum
    case Arr(value) => value.map(findNumbers(pred)).sum
    case Num(value) => value
    case _          => 0.0

  def ignoreRed(json: Value): Boolean = json match
    case Obj(value) =>
      value.values.exists: v =>
        v.match
          case Str(value) => value == "red"
          case _          => false
    case _ => false

  def solve1(line: String) = findNumbers(_ => false)(read(line))
  def solve2(line: String) = findNumbers(ignoreRed)(read(line))

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "12" / "12.test.input.txt")
  lazy val result1 = lines map Solving.solve1
  lazy val result2 = lines map Solving.solve2
// Testing.result1 // part 1: 15,6,6,6,3,3,0,0,0,0
// Testing.result2 // part 2:  0,6,6,4,3,3,0,0,0,0

object Main:
  private lazy val line = os.read.lines(os.pwd / "2015" / "12" / "12.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part 1: 111754
// Main.result2 // part 2: 65402
