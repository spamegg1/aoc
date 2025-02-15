package aoc2017.day15

object DataDefs:
  val divisor = 2147483647L

  extension (start: Long)
    def generator(factor: Long): Iterator[Long] =
      Iterator.iterate(start)(n => (n * factor) % divisor).drop(1)
    def judge(that: Long): Boolean = (start & 0xffff) == (that & 0xffff)

object Solving:
  import DataDefs.*

  def solve1(pairs: Int)(aStart: Long, aFactor: Long, bStart: Long, bFactor: Long) =
    val a = aStart.generator(aFactor)
    val b = bStart.generator(bFactor)
    a.zip(b).take(pairs).count(_.judge(_))

  def solve2(pairs: Int)(aStart: Long, aFactor: Long, bStart: Long, bFactor: Long) =
    val a = aStart.generator(aFactor)
    val b = bStart.generator(bFactor)
    a
      .filter(_ % 4 == 0)
      .zip(b.filter(_ % 8 == 0))
      .take(pairs)
      .count(_.judge(_))

object Test:
  lazy val res1 = Solving.solve1(40000000)(65L, 16807L, 8921L, 48271L)
  lazy val res2 = Solving.solve2(5000000)(65L, 16807L, 8921L, 48271L)

object Main:
  lazy val res1 = Solving.solve1(40000000)(116L, 16807L, 299L, 48271L)
  lazy val res2 = Solving.solve2(5000000)(116L, 16807L, 299L, 48271L)

@main
def run: Unit =
  println(Test.res1) // part 1: 588
  println(Test.res2) // part 2: 309
  println(Main.res1) // part 1: 569
  println(Main.res2) // part 2: 298
