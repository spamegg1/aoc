package aoc2018.day11

object DataDefs:
  type Power     = Int
  type Pos       = (x: Int, y: Int)
  type FuelCells = Map[Pos, Power]
  type PowerGrid = Seq[(Int, Pos, Power)]

  extension (p: Pos)
    def jumpBy(x: Int, y: Int) = (x = p.x + x, y = p.y + y)
    def powerLevel(serial: Power) =
      (((p.x + 10) * p.y + serial) * (p.x + 10) / 100) % 10 - 5

  extension (fc: FuelCells)
    def areaTable(sqSize: Int, gridSize: Int): FuelCells = Pos
      .grid(sqSize, gridSize)
      .foldLeft(Map.empty[Pos, Power].withDefaultValue(0)): (table, pos) =>
        val area =
          fc(pos) +
            table(pos.jumpBy(-1, 0)) +
            table(pos.jumpBy(0, -1)) -
            table(pos.jumpBy(-1, -1))
        table.updated(pos, area)

    def powerGrid(sqSize: Int, gridSize: Int): PowerGrid = Pos
      .grid(sqSize, gridSize)
      .map: pos =>
        val area =
          fc(pos) -
            fc(pos.jumpBy(-sqSize, 0)) -
            fc(pos.jumpBy(0, -sqSize)) +
            fc(pos.jumpBy(-sqSize, -sqSize))
        (sqSize, pos, area)

  object Pos:
    def grid(sqSize: Int, gridSize: Int) =
      for
        x <- sqSize to gridSize
        y <- sqSize to gridSize
      yield (x = x, y = y)

  object FuelCells:
    def make(sqSize: Int, gridSize: Int, serial: Power): FuelCells =
      Pos.grid(sqSize, gridSize).map(pos => pos -> pos.powerLevel(serial)).toMap

object Solving:
  import DataDefs.*

  def solve1(sqSize: Int, gridSize: Int)(serial: Int) =
    val fuelCells         = FuelCells.make(sqSize, gridSize, serial)
    val table             = fuelCells.areaTable(sqSize, gridSize)
    val candidates        = table.powerGrid(sqSize, gridSize)
    val (size, pos, area) = candidates.maxBy(_._3)
    Seq(pos.x - size + 1, pos.y - size + 1).mkString(",")

  def solve2(sqSize: Int, gridSize: Int)(serial: Int) =
    val fuelCells  = FuelCells.make(sqSize, gridSize, serial)
    val table      = fuelCells.areaTable(sqSize, gridSize)
    val candidates = for size <- sqSize to gridSize yield table.powerGrid(size, gridSize)
    val (size, pos, area) = candidates.flatten.maxBy(_._3)
    Seq(pos.x - size + 1, pos.y - size + 1, size).mkString(",")

object Test:
  lazy val serials = Seq(18, 42)
  lazy val res1    = serials map Solving.solve1(3, 300)
  lazy val res2    = serials map Solving.solve2(1, 300)

object Main:
  lazy val res1 = Solving.solve1(3, 300)(8444)
  lazy val res2 = Solving.solve2(1, 300)(8444)

@main
def run: Unit =
  println(Test.res1) // part 1: ((33,45),29),((21,61),30)
  println(Test.res2) // part 2: (16,((90,269),113)),(12,((232,251),119))
  println(Main.res1) // part 1: ((243,68),28)
  println(Main.res2) // part 2: (12,((236,252),96))
