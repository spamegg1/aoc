package aoc2025.day01

object DataDefs:
  val Mod     = 100
  val Initial = 50
  type Instr = (dir: Int, clicks: Int) // dir = ±1

  case class Dial(pointer: Int, hits: Int, passes: Int):
    def next(instr: Instr) =
      val clicksToReachZero =
        if instr.dir == -1 then (if pointer == 0 then Mod else pointer)
        else Mod - pointer
      val newPointer          = (pointer + instr.dir * instr.clicks) mod Mod
      val newHits             = hits + (if newPointer == 0 then 1 else 0)
      val passCount           = (instr.clicks - clicksToReachZero) / Mod + 1
      val passZeroAtLeastOnce = instr.clicks >= clicksToReachZero
      val newPasses           = passes + (if passZeroAtLeastOnce then passCount else 0)
      Dial(newPointer, newHits, newPasses)

  extension (n: Int)
    infix def mod(modulo: Int) =
      val res = n % modulo
      res + (if res < 0 then modulo else 0)

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Instr = line match
    case s"L$value" => (dir = -1, clicks = value.toInt)
    case s"R$value" => (dir = 1, clicks = value.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) = Parsing
    .parse(lines)
    .foldLeft(Dial(Initial, 0, 0)): (dial, instr) =>
      dial.next(instr)

object Test:
  val file   = os.pwd / "2025" / "01" / "01.test.input.txt"
  val lines  = os.read.lines(file)
  val solved = Solving.solve(lines)
  val res1   = solved.hits
  val res2   = solved.passes

object Main:
  val file   = os.pwd / "2025" / "01" / "01.input.txt"
  val lines  = os.read.lines(file)
  val solved = Solving.solve(lines)
  val res1   = solved.hits
  val res2   = solved.passes

@main
def run: Unit =
  println(Test.res1) // part 1: 3
  println(Test.res2) // part 2: 6
  println(Main.res1) // part 1: 1048
  println(Main.res2) // part 2: 6498
