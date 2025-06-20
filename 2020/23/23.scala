package aoc2020.day23

object DataDefs:
  case class Cup(value: Int, var next: Option[Cup] = None)

object Solving:
  import DataDefs.*

  def play(seq: Seq[Int], rounds: Int): Cup =
    val cups = seq
      .scanRight(0 -> Cup(0)):
        case (value, (nextValue, nextCup)) =>
          value -> Cup(value, Some(nextCup))
      .toMap
    cups(seq.last).next = Some(cups(seq.head))

    @annotation.tailrec
    def helper(cup: Cup, step: Int): Cup =
      if step == rounds then cups(1)
      else
        val first  = cup.next.get
        val second = first.next.get
        val third  = second.next.get
        val picked = Set(cup.value, first.value, second.value, third.value)
        val destination = Iterator
          .iterate(cup.value)(n => if n == 1 then seq.size else n - 1)
          .dropWhile(picked.contains)
          .next()
        cup.next = third.next
        third.next = cups(destination).next
        cups(destination).next = Some(first)
        helper(cup.next.get, step + 1)
    end helper
    helper(cups(seq.head), 0)

  def solve1(num: Int)(rounds: Int) =
    val digits = num.toString.map(_.asDigit)
    val start  = play(digits, rounds)
    @annotation.tailrec
    def helper(cup: Cup, seq: Seq[Int]): String =
      if cup == start then seq.mkString
      else helper(cup.next.get, seq.appended(cup.value))
    helper(start.next.get, Seq())

  def solve2(num: Int)(rounds: Int)(max: Int) =
    val digits = num.toString.map(_.asDigit) ++ (10 to max)
    val cup    = play(digits, rounds)
    val first  = cup.next.get
    val second = first.next.get
    first.value.toLong * second.value.toLong

object Test:
  lazy val res11 = Solving.solve1(389125467)(10)
  lazy val res12 = Solving.solve1(389125467)(100)
  lazy val res2  = Solving.solve2(389125467)(10000000)(1000000)

object Main:
  lazy val res1 = Solving.solve1(589174263)(100)
  lazy val res2 = Solving.solve2(589174263)(10000000)(1000000)

@main
def run: Unit =
  println(Test.res11) // part 1: 92658374
  println(Test.res12) // part 1: 67384529
  println(Test.res2)  // part 2: 149245887792
  println(Main.res1)  // part 1: 43896725
  println(Main.res2)  // part 2: 2911418906
