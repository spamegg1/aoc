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
    val decomp        = primeDecomposition(house)
    val listsOfPowers = decomp.map((prime, exp) => (0 to exp).map(math.pow(prime, _)))
    listsOfPowers.tail.foldLeft(listsOfPowers.head): (res, nextList) =>
      for
        pow1 <- res
        pow2 <- nextList
      yield pow1 * pow2

  def getRelevantDivisors(house: Int) = allDivisors(house).filter(house <= _ * 50L).sum

  def solve2(target: Long) =
    var house = 500000
    while getRelevantDivisors(house) * 11L < target do house += 1
    house

object Test:
  lazy val res1 = Solving.solve1(150L)
  lazy val res2 = Solving.solve2(150L)
// Test.res1 // part 1: 8
// Test.res2 // part 2: 8

object Main:
  lazy val res1 = Solving.solve1(33100000L)
  lazy val res2 = Solving.solve2(33100000L)
// Main.res1 // part 1: 776160
// Main.res2 // part 2: 786240
