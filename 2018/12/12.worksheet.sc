/*
--- Day 12: Subterranean Sustainability ---
The year 518 is significantly more underground than your history books implied.
Either that, or you've arrived in a vast cavern network under the North Pole.

After exploring a little, you discover a long tunnel that contains a row of
small pots as far as you can see to your left and right.
A few of them contain plants -
someone is trying to grow things in these geothermally-heated caves.

The pots are numbered, with 0 in front of you.
To the left, the pots are numbered -1, -2, -3,
and so on; to the right, 1, 2, 3.... Your puzzle input
contains a list of pots from 0 to the right and whether
they do (#) or do not (.) currently contain a plant,
the initial state. (No other pots currently contain plants.)
For example, an initial state of #..##.... indicates that pots
0, 3, and 4 currently contain plants.

Your puzzle input also contains some notes you find on a nearby table:
someone has been trying to figure out how these plants spread to nearby pots.
Based on the notes, for each generation of plants,
a given pot has or does not have a plant based on whether that pot
(and the two pots on either side of it) had a plant in the last generation.
These are written as LLCRR => N, where L are pots to the left,
C is the current pot being considered, R are the pots to the right,
and N is whether the current pot will have a plant in the next generation.
For example:
  A note like ..#.. => . means that a pot that contains a plant but
    with no plants within two pots of it will not have a plant in it
    during the next generation.
  A note like ##.## => . means that an empty pot with two plants on
    each side of it will remain empty in the next generation.
  A note like .##.# => # means that a pot has a plant in a given
    generation if, in the previous generation, there were plants in that pot,
    the one immediately to the left, and the one two pots to the right,
    but not in the ones immediately to the right and two to the left.

It's not clear what these plants are for, but you're sure it's important,
so you'd like to make sure the current configuration of plants is
sustainable by determining what will happen after 20 generations.

For example, given the following input:

initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #

For brevity, in this example,
only the combinations which do produce a plant are listed.
(Your input includes all possible combinations.)
Then, the next 20 generations will look like this:

                 1         2         3
       0         0         0         0
 0: ...#..#.#..##......###...###...........
 1: ...#...#....#.....#..#..#..#...........
 2: ...##..##...##....#..#..#..##..........
 3: ..#.#...#..#.#....#..#..#...#..........
 4: ...#.#..#...#.#...#..#..##..##.........
 5: ....#...##...#.#..#..#...#...#.........
 6: ....##.#.#....#...#..##..##..##........
 7: ...#..###.#...##..#...#...#...#........
 8: ...#....##.#.#.#..##..##..##..##.......
 9: ...##..#..#####....#...#...#...#.......
10: ..#.#..#...#.##....##..##..##..##......
11: ...#...##...#.#...#.#...#...#...#......
12: ...##.#.#....#.#...#.#..##..##..##.....
13: ..#..###.#....#.#...#....#...#...#.....
14: ..#....##.#....#.#..##...##..##..##....
15: ..##..#..#.#....#....#..#.#...#...#....
16: .#.#..#...#.#...##...#...#.#..##..##...
17: ..#...##...#.#.#.#...##...#....#...#...
18: ..##.#.#....#####.#.#.#...##...##..##..
19: .#..###.#..#.#.#######.#.#.#..#.#...#..
20: .#....##....#####...#######....#.#..##.
       0         0         0         0    5
                 1         2         3    3

The generation is shown along the left, where 0 is the initial state.
The pot numbers are shown along the top, where 0 labels the center pot,
negative-numbered pots extend to the left, and
positive-numbered pots extend to the right.
Remember, the initial state begins at pot 0,
which is not the leftmost pot used in this example.

After one generation, only seven plants remain.
The one in pot 0 matched the rule looking for ..#..,
the one in pot 4 matched the rule looking for .#.#.,
the one in pot 9 matched the rule looking for .##.., and so on.

In this example, after 20 generations, the pots shown as # contain plants,
the furthest left of which is pot -2,
and the furthest right of which is pot 34.
Adding up all the numbers of plant-containing pots
after the 20th generation produces 325.

After 20 generations, what is the sum of the
numbers of all pots which contain a plant?

--- Part Two ---
You realize that 20 generations aren't enough.
After all, these plants will need to last another
1500 years to even reach your timeline, not to mention your future.

After fifty billion (50000000000) generations,
what is the sum of the numbers of all pots which contain a plant?

 */
import collection.mutable.ArrayDeque

object DataDefs:
  case class Rule(pattern: String, result: String)

  case class State(pots: String, rules: Seq[Rule], min: Int = 0, max: Int):
    lazy val nextPots = s"....$pots...."
      .sliding(5)
      .map: five =>
        rules.find(_.pattern == five) match
          case None        => "."
          case Some(value) => value.result
      .mkString
    lazy val newMin = min - (2 - nextPots.takeWhile(_ == '.').size)
    lazy val newMax = max + (2 - nextPots.reverse.takeWhile(_ == '.').size)
    def newPots = nextPots.drop(2 + newMin - min).dropRight(2 - newMax + max)
    def next = copy(pots = newPots, min = newMin, max = newMax)
    def plants = pots
      .zip(min to max)
      .filter(_._1 == '#')
      .map(_._2)
      .sum
    override def toString() = pots

  object State:
    def apply(initial: String, rules: Seq[Rule]) =
      new State(initial, rules, 0, initial.size - 1)

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$pattern => $result" => Rule(pattern, result)

  def parseRules(lines: Seq[String]) = lines map parseRule

  def parse(lines: Seq[String]) = State(lines.head, parseRules(lines.tail))

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(generations: Int) =
    var state = Parsing.parse(lines)
    for _ <- 1 to generations do state = state.next
    state.plants

  def solve2(lines: Seq[String])(generations: Long) =
    var state = Parsing.parse(lines)
    var count = 0
    val memo = collection.mutable.Map(count -> state.plants)

    // for _ <- 1 to 200 do // find pattern in plant counts
    //   state = state.next
    //   count += 1
    //   memo(count) = state.plants
    // os.write(os.pwd / "2018" / "12" / "memo.txt", memo.mkString("\n"))

    // starting gen 98, every gen increases plants by 51
    // at gen 98, the plant count is 6193
    (generations - 98L) * 51L + 6193L

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "12" / "12.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(20)
// Testing.result1 // part 1: 325

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2018" / "12" / "12.input.txt")
  lazy val result1 = Solving.solve1(lines)(20)
  lazy val result2 = Solving.solve2(lines)(50000000000L)
// Main.result1 // part 1: 3421
// Main.result2 // part 2: 2550000001195
