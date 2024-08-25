/*
--- Day 17: No Such Thing as Too Much ---
The elves bought too much eggnog again - 150 liters this time.
To fit it all into your refrigerator,
you'll need to move it into smaller containers.
You take an inventory of the capacities of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
If you need to store 25 liters, there are four ways to do it:
  15 and 10
  20 and 5 (the first 5)
  20 and 5 (the second 5)
  15, 5, and 5

Filling all containers entirely, how many different combinations
of containers can exactly fit all 150 liters of eggnog?

--- Part Two ---
While playing with all the containers in the kitchen,
another load of eggnog arrives!
The shipping and receiving department is requesting as many containers as you can spare.

Find the minimum number of containers that can exactly fit all 150 liters of eggnog.
How many different ways can you fill that number
of containers and still hold exactly 150 litres?

In the example above, the minimum number of containers was two.
There were three ways to use that many containers,
and so the answer there would be 3.
 */
object Parsing:
  // combinations removes repetitions, so we zipWithIndex to make them distinct.
  def parse(lines: Seq[String]) = lines.map(_.toInt).zipWithIndex

object Solving:
  def powerSet(containers: Seq[(Int, Int)]) = // brute force attempt
    (0 until containers.size).flatMap(containers.combinations(_))

  def solve1(lines: Seq[String])(eggnog: Int) =
    val containers = Parsing.parse(lines)
    powerSet(containers).count(set => set.map(_._1).sum == eggnog)

  def solve2(lines: Seq[String])(eggnog: Int) =
    val containers = Parsing.parse(lines)
    val good = powerSet(containers).filter(set => set.map(_._1).sum == eggnog)
    val minSize = good.minBy(_.size).size
    good.count(_.size == minSize)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "17" / "17.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(25)
  lazy val result2 = Solving.solve2(lines)(25)
// Testing.result1 // part 1: 4
// Testing.result2 // part 2: 3

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "17" / "17.input.txt")
  lazy val result1 = Solving.solve1(lines)(150)
  lazy val result2 = Solving.solve2(lines)(150)
// Main.result1 // part 1: 654
// Main.result2 // part 2: 57
