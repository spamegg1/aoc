/*
--- Day 5: Alchemical Reduction ---
You've managed to sneak in to the prototype suit manufacturing lab.
The Elves are making decent progress, but are still struggling with
the suit's size reduction capabilities.

While the very latest in 1518 alchemical technology might have solved
their problem eventually, you can do better. You scan the chemical
composition of the suit's material and discover that it is formed by
extremely long polymers (one of which is available as your puzzle input).

The polymer is formed by smaller units which, when triggered, react with
each other such that two adjacent units of the same type and opposite
polarity are destroyed. Units' types are represented by letters; units'
polarity is represented by capitalization. For instance, r and R are
units with the same type but opposite polarity, whereas r and s are
entirely different types and do not react.

For example:
  In aA, a and A react, leaving nothing behind.
  In abBA, bB destroys itself, leaving aA. As above, this then destroys itself,
    leaving nothing.
  In abAB, no two adjacent units are of the same type, and so nothing happens.
  In aabAAB, even though aa and AA are of the same type, their polarities match,
    and so nothing happens.

Now, consider a larger example, dabAcCaCBAcCcaDA:

dabAcCaCBAcCcaDA  The first 'cC' is removed.
dabAaCBAcCcaDA    This creates 'Aa', which is removed.
dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
dabCBAcaDA        No further actions can be taken.

After all possible reactions, the resulting polymer contains 10 units.
How many units remain after fully reacting the polymer you scanned?

--- Part Two ---
Time to improve the polymer.
One of the unit types is causing problems; it's preventing the polymer from
collapsing as much as it should. Your goal is to figure out which unit type is
causing the most problems, remove all instances of it (regardless of polarity),
fully react the remaining polymer, and measure its length.

For example, again using the polymer dabAcCaCBAcCcaDA from above:
  Removing all A/a units produces dbcCCBcCcD.
    Fully reacting this polymer produces dbCBcD, which has length 6.
  Removing all B/b units produces daAcCaCAcCcaDA.
    Fully reacting this polymer produces daCAcaDA, which has length 8.
  Removing all C/c units produces dabAaBAaDA.
    Fully reacting this polymer produces daDA, which has length 4.
  Removing all D/d units produces abAcCaCBAcCcaA.
    Fully reacting this polymer produces abCBAc, which has length 6.

In this example, removing all C/c units was best, producing the answer 4.
What is the length of the shortest polymer you can produce by removing all units
of exactly one type and fully reacting the result?
 */
object DataDefs:
  val pairs = ('a' to 'z').zip('A' to 'Z') ++ ('A' to 'Z').zip('a' to 'z')
  val polars = pairs map ((c1, c2) => s"$c1$c2")

  extension (units: String)
    def reactOnce(types: Seq[String]): String =
      var index = 0
      var result = ""
      while index < units.size do
        if index == units.size - 1 then
          result = result.appended(units(index))
          index += 1
        else
          val pair = units.slice(index, index + 2)
          if types contains pair then index += 2
          else
            result = result.appended(pair.head)
            index += 1
      result

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*

  def solver(line: String)(types: Seq[String]) =
    var first = line
    var second = first.reactOnce(types)
    while first != second do
      first = second
      second = second.reactOnce(types)
    second.length

  def solve1(line: String) = solver(line)(polars)
  def solve2(line: String) =
    polars
      .map: polar =>
        val filteredLine = line.filterNot(polar.contains(_))
        solver(filteredLine)(polars)
      .min

object Testing:
  lazy val line = os.read.lines(os.pwd / "2018" / "05" / "05.test.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Testing.result1 // part 1: 10
// Testing.result2 // part 2: ???

object Main:
  lazy val line = os.read.lines(os.pwd / "2018" / "05" / "05.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part 1: 9348
// Main.result2 // part 2: 4996
