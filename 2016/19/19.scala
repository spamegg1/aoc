package aoc2016.day19

object DataDefs:
  case class Elf(pos: Int, var next: Option[Elf]) // part 2

object Solving:
  import DataDefs.*
  // https://en.wikipedia.org/wiki/Josephus_problem
  // k = 2 case, recurrence relation f(2n) = 2f(n) + 1
  // Solution: if n = 2^m + l with 0 <= l < 2^m, then f(n) = 2l + 1.
  // So, f(n) = 2*(n - 2^floor(log_2(n))) + 1.
  // def solve1(n: Int) =
  //   val m = math.floor(math.log(n) / math.log(2)).toInt
  //   2 * (n - math.pow(2, m).toInt) + 1

  // more efficient solution: if elves = 1abc...z in binary, then answer = abc...z1.
  // do 1abc...z - 1000...0 = 0abc...z, then left-shift by 1: abc...z0, then add 1.
  def solve1(elves: Int) = ((elves - Integer.highestOneBit(elves)) << 1) + 1

  def solve2(elves: Int) =
    var elf = Elf(1, None) // elf = elf1
    elf.next = Some(elf) // elf1(elf1)

    for n <- 2 to elves do
      val nextElf = Elf(n, elf.next) // e2->e1, e3->e1, ...
      elf.next = Some(nextElf) // e1->e2, e2->e3, ...
      elf = nextElf            // elf = e2, elf = e3, ...
    // at the end we have: e1 -> e2 -> e3 -> ... -> en -> e1, elf = en

    for i <- 1 to elves / 2 do elf = elf.next.get // now elf = en/2->en/2+1

    for i <- elves to 2 by -1 do
      elf.next = elf.next.get.next
      if i % 2 == 1 then elf = elf.next.get

    elf.pos

object Test:
  lazy val res1 = Seq(5, 6, 9, 10) map Solving.solve1
  lazy val res2 = Seq(5, 6, 9, 10) map Solving.solve2

object Main:
  lazy val res1 = Solving.solve1(3014387)
  lazy val res2 = Solving.solve2(3014387)

@main
def run: Unit =
  println(Test.res1) // part 1: 3, 5, 3, 5
  println(Test.res2) // part 2: 2, 3, 9, 1
  println(Main.res1) // part 1: 1834471
  println(Main.res2) // part 2: 1420064
