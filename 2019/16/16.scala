package aoc2019.day16

object Parsing:
  def parse1(line: String) = line.map(_.asDigit)
  def parse2(line: String)(repeats: Int) =
    val index = line.take(7).toInt
    (line * repeats).drop(index).map(_.asDigit)

object Solving:
  def phase1(input: Seq[Int]): Seq[Int] = (0 until input.size).map: n =>
    val raw = input.zipWithIndex.map: (next, index) =>
      ((index + 1) / (n + 1)) % 4 match
        case 0 | 2 => 0
        case 1     => next
        case 3     => -next
    (raw.sum % 10).abs

  def phase2(input: Seq[Int]): Seq[Int] = input.scanRight(0)(_ + _).map(_ % 10)

  def solve(digits: Seq[Int])(fun: Seq[Int] => Seq[Int])(phases: Int) = Iterator
    .iterate(digits)(fun)
    .drop(phases)
    .next()
    .take(8)
    .mkString

  def solve1(phases: Int)(line: String) = solve(Parsing.parse1(line))(phase1)(phases)
  def solve2(phases: Int)(repeats: Int)(line: String) =
    solve(Parsing.parse2(line)(repeats))(phase2)(phases)

object Test:
  lazy val file  = os.pwd / "2019" / "16" / "16.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1(100)
  lazy val res2  = lines map Solving.solve2(100)(10000)

object Main:
  lazy val file = os.pwd / "2019" / "16" / "16.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(100)(line)
  lazy val res2 = Solving.solve2(100)(10000)(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 24176176,73745418,52432133,_,_,_
  println(Test.res2) // part 2: _,_,_,84462026,78725270,53553731
  println(Main.res1) // part 1: 63483758
  println(Main.res2) // part 2: 96099551
