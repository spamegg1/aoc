/*
--- Day 20: Infinite Elves and Infinite Houses ---
To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door.
He sends them down a street with infinite houses numbered sequentially:
1, 2, 3, 4, 5, and so on.

Each Elf is assigned a number, too, and delivers presents to houses based on that number:
  The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
  The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
  Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....

There are infinitely many Elves, numbered starting with 1.
Each Elf delivers presents equal to ten times his or her number at each house.

So, the first nine houses on the street end up like this:
House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.

The first house gets 10 presents: it is visited only by Elf 1,
which delivers 1 * 10 = 10 presents.
The fourth house gets 70 presents, because it is visited by Elves 1, 2, and 4,
for a total of 10 + 20 + 40 = 70 presents.

What is the lowest house number of the house to get at
least as many presents as the number in your puzzle input?

-- Part Two ---
The Elves decide they don't want to visit an infinite number of houses.
Instead, each Elf will stop after delivering presents to 50 houses.
To make up for it, they decide to deliver presents equal to eleven times
their number at each house.

With these changes, what is the new lowest house number of the house
to get at least as many presents as the number in your puzzle input?
 */
import org.apache.commons.numbers.primes.Primes.primeFactors
import scala.jdk.CollectionConverters.ListHasAsScala

object Solving:
  def primeDecomposition(house: Int) =
    primeFactors(house).asScala.groupMapReduce(_.toInt)(_ => 1)(_ + _)

  def sumOfDivisors(house: Int): Long = primeDecomposition(house)
    .map((prime, exp) => (math.pow(prime, exp + 1) - 1) / (prime - 1))
    .product
    .toLong

  def solve1(target: Long) =
    var house = 500000
    while sumOfDivisors(house) * 10L < target do house += 1
    house

  def allDivisors(house: Int) =
    val decomp = primeDecomposition(house)
    val listsOfPowers = decomp.map((prime, exp) => (0 to exp).map(math.pow(prime, _)))
    listsOfPowers.tail.foldLeft(listsOfPowers.head): (result, nextList) =>
      for
        pow1 <- result
        pow2 <- nextList
      yield pow1 * pow2

  def getRelevantDivisors(house: Int) = allDivisors(house).filter(house <= _ * 50L).sum

  def solve2(target: Long) =
    var house = 500000
    while getRelevantDivisors(house) * 11L < target do house += 1
    house

object Testing:
  lazy val result1 = Solving.solve1(150L)
  lazy val result2 = Solving.solve2(150L)
// Testing.result1 // part 1: 8
// Testing.result2 // part 2: 8

object Main:
  lazy val result1 = Solving.solve1(33100000L)
  lazy val result2 = Solving.solve2(33100000L)
// Main.result1 // part 1: 776160
// Main.result2 // part 2: 786240
