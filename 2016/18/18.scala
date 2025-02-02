package aoc2016.day18

object DataDefs:
  enum Tile:
    case Safe, Trap
    def toChar = this match
      case Safe => '.'
      case Trap => '^'
  import Tile.*

  extension (c: Char)
    def toTile = c match
      case '.' => Safe
      case _   => Trap

  extension (tiles: List[Tile])
    def nextTile = tiles match
      case Trap :: Trap :: Safe :: Nil => Trap
      case Safe :: Trap :: Trap :: Nil => Trap
      case Trap :: Safe :: Safe :: Nil => Trap
      case Safe :: Safe :: Trap :: Nil => Trap
      case _                           => Safe

    def padded    = Safe +: tiles :+ Safe
    def next      = tiles.padded.sliding(3).map(_.nextTile).toList
    def safeCount = tiles.count(_ == Safe)
    // def toString = tiles.map(_.toChar).mkString

  case class TileIter(total: Int, tiles: List[Tile]):
    def next = TileIter(total + tiles.safeCount, tiles.next)

object Parsing:
  import DataDefs.*
  def parse(line: String) = line.map(_.toTile).toList

object Solving:
  import DataDefs.*

  def solve(line: String)(rowCount: Int) =
    val start = TileIter(0, Parsing.parse(line))
    (0 until rowCount)
      .foldLeft(start)((tileIter, _) => tileIter.next)
      .total

object Test:
  lazy val file = os.pwd / "2016" / "18" / "18.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res  = Solving.solve(line)(10)

object Main:
  lazy val file = os.pwd / "2016" / "18" / "18.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve(line)(40)
  lazy val res2 = Solving.solve(line)(400000)

@main
def run: Unit =
  println(Test.res)  // part 1: 38
  println(Main.res1) // part 1: 1982
  println(Main.res2) // part 2: 20005203 a bit slow
