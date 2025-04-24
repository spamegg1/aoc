package aoc2020.day15

object DataDefs:
  case class Game(spoken: Map[Int, Int], last: (Int, Int), turn: Int):
    lazy val speak = last._2 - spoken.getOrElse(last._1, last._2)
    lazy val next  = Game(spoken + last, (speak, turn), turn + 1)

  object Game:
    def apply(starting: Seq[Int]) =
      val spoken = starting.zip(1 to starting.size)
      new Game(spoken.init.toMap, spoken.last, spoken.size + 1)

object Solving:
  import DataDefs.*

  def solve(num: Int)(nums: Seq[Int]) =
    var game = Game(nums)
    var turn = game.turn
    while turn <= num do
      game = game.next
      turn = game.turn
    game.last._1

object Test:
  private lazy val nums = Seq(
    Seq(0, 3, 6),
    Seq(1, 3, 2),
    Seq(2, 1, 3),
    Seq(1, 2, 3),
    Seq(2, 3, 1),
    Seq(3, 2, 1),
    Seq(3, 1, 2)
  )
  lazy val res1 = nums map Solving.solve(2020)
  lazy val res2 = nums map Solving.solve(30000000)

object Main:
  lazy val inp  = Seq(8, 13, 1, 0, 18, 9)
  lazy val res1 = Solving.solve(2020)(inp)
  lazy val res2 = Solving.solve(30000000)(inp)

@main
def run: Unit =
  println(Test.res1) // part 1: 436, 1, 10, 27, 78, 438, 1836
  // println(Test.res2) // part 2: 175594, 2578, 3544142, 261214, 6895259, 18, 362
  println(Main.res1) // part 1: 755
  println(Main.res2) // part 2: 11962
