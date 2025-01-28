package aoc2016.day19

object DataDefs:
  ???

object Solving:
  import DataDefs.*

  def solve1(elves: Int) =
    0

  def solve2(elves: Int) =
    0

object Test:
  lazy val res1 = Seq(5, 6, 9, 10) map Solving.solve1
  lazy val res2 = Seq(5, 6, 9, 10) map Solving.solve2

object Main:
  lazy val res1 = Solving.solve1(3014387)
  lazy val res2 = Solving.solve2(30000)

@main
def run: Unit =
  println(Test.res1) // part 1: 3, 5, 3, 5
  println(Test.res2) // part 2: 2, 3, 9, 1
  println(Main.res1) // part 1: 1834471 too slow, 30 sec
  println(Main.res2) // part 2:
