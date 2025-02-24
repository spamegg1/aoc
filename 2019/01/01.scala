package aoc2019.day01

object DataDefs:
  type Fuel = Long

  case class Mass(mass: Long):
    def fuel: Fuel = mass / 3 - 2 // part 1

    @annotation.tailrec
    private def recursiveFuel(f: Fuel, acc: Fuel): Fuel = // part 2
      val newFuel = f / 3 - 2
      if newFuel <= 0 then acc
      else recursiveFuel(newFuel, acc + newFuel)

    def moreFuel = recursiveFuel(mass, 0L)

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Seq[Mass] = lines.map(line => Mass(line.toLong))

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]): Fuel = Parsing.parse(lines).map(_.fuel).sum
  def solve2(lines: Seq[String]): Fuel = Parsing.parse(lines).map(_.moreFuel).sum

object Test:
  lazy val file  = os.pwd / "2019" / "01" / "01.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2019" / "01" / "01.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 34241
  println(Test.res2) // part 2: 51316
  println(Main.res1) // part 1: 3448043
  println(Main.res2) // part 2: 5169198
