package aoc2019.day08

object DataDefs:
  case class Dim(w: Int, h: Int):
    def size = w * h

  extension (s: String)
    def seeThru: Char = s.foldLeft('2'): (res, char) =>
      if res == '2' then char else res

  case class Layer(rows: Seq[String])(using d: Dim):
    def count(num: Char) = rows.mkString.count(_ == num)
    def part1            = count('1') * count('2')
    def show = rows
      .map: row =>
        row.map {
          case '0' => '.'
          case '1' => '#'
        }
      .mkString("\n")

    def collate = Layer:
      for index <- 0 until d.w yield (for row <- rows yield row(index)).mkString

    def pixels = rows.map(_.seeThru).mkString // do not re-collate from Layers

  case class Layers(layers: Seq[Layer])(using d: Dim):
    def transpose: Seq[Layer] =
      for index <- 0 until d.h yield Layer(for layer <- layers yield layer.rows(index))
    def collated: Seq[Layer] = transpose.map(_.collate) // do not re-collate in Layer
    def pixels               = Layer(collated.map(_.pixels)).show

object Parsing:
  import DataDefs.*

  def parse(line: String)(using d: Dim) = Layers:
    line
      .grouped(d.size)
      .map(it => Layer(it.grouped(d.w).toSeq))
      .toSeq

object Solving:
  import DataDefs.*

  def solve1(line: String)(using d: Dim) = Parsing
    .parse(line)
    .layers
    .minBy(_.count('0'))
    .part1

  def solve2(line: String)(using d: Dim) = Parsing.parse(line).pixels

object Test:
  import DataDefs.*
  given d: Dim  = Dim(2, 2)
  lazy val file = os.pwd / "2019" / "08" / "08.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

object Main:
  import DataDefs.*
  given d: Dim  = Dim(25, 6)
  lazy val file = os.pwd / "2019" / "08" / "08.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 4
  println(Test.res2) // part 2: 0110
  println(Main.res1) // part 1: 2318
  println(Main.res2) // part 2: AHFCB
