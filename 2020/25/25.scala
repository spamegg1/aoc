package aoc2020.day25

object DataDefs:
  val divisor = 20201227L

object Solving:
  import DataDefs.*

  def solve(nums: Seq[Long]) =
    val (cardPub, doorPub) = (nums.head, nums.last)
    val doorLoopSize = Iterator
      .iterate(1L)(n => (n * 7) % divisor)
      .indexWhere(_ == doorPub)
    BigInt(cardPub).modPow(doorLoopSize, divisor)

object Test:
  val res = Solving.solve(Seq(5764801L, 17807724L))

object Main:
  val res = Solving.solve(Seq(10212254L, 12577395L))

@main
def run: Unit =
  println(Test.res) // part 1: 14897079
  println(Main.res) // part 1: 290487
