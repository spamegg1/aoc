package aoc2025.day04

object DataDefs:
  val Deltas = Seq(-1, 0, 1)
  val Limit  = 4
  val Roll   = '@'
  val Access = 'x'
  val Empty  = '.'
  val Chars  = Set(Roll, Access)

  case class Grid(grid: Array[Array[Char]]):
    def accessible: Int =
      var count = 0
      for
        row <- grid.indices
        col <- grid(row).indices
        if grid(row)(col) == Roll
        nearby =
          for
            di <- Deltas
            dj <- Deltas
            if di != 0 || dj != 0
            char = grid
              .lift(row + di)
              .flatMap(_.lift(col + dj))
              .getOrElse(Empty)
          yield char
      do
        val neighbors = nearby.count(Chars.contains)
        if neighbors < Limit then
          grid(row)(col) = Access
          count += 1
      count

    def removeRolls =
      var count = 0
      var done  = false
      while !done do
        done = true
        count += accessible
        for
          row <- grid.indices
          col <- grid(row).indices
          if grid(row)(col) == Access
        do
          grid(row)(col) = Empty
          done = false
      count

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = Grid(lines.map(_.toCharArray).toArray)

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = Parsing.parse(lines).accessible
  def solve2(lines: Seq[String]) = Parsing.parse(lines).removeRolls

object Test:
  val file  = os.pwd / "2025" / "04" / "04.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2025" / "04" / "04.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 13
  println(Test.res2) // part 2: 43
  println(Main.res1) // part 1: 1564
  println(Main.res2) // part 2: 9401
