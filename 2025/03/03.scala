package aoc2025.day03

object Solving:
  @annotation.tailrec
  def joltage(batteries: Int)(acc: Long)(bank: String): Long =
    if batteries == 0 then acc
    else
      val digit = bank.take(bank.length - batteries + 1).map(_.asDigit).max
      val index = bank.indexOf(digit.toString)
      joltage(batteries - 1)(acc * 10 + digit)(bank.drop(index + 1))

  def solve(digits: Int)(lines: Seq[String]) = lines.map(joltage(digits)(0L)).sum

  val solve1 = solve(2)
  val solve2 = solve(12)

object Test:
  val file  = os.pwd / "2025" / "03" / "03.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2025" / "03" / "03.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 357
  println(Test.res2) // part 2: 3121910778619
  println(Main.res1) // part 1: 16842
  println(Main.res2) // part 2: 167523425665348
