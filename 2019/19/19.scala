package aoc2019.day19

import aoc2019.day09.DataDefs.*

object Parsing:
  def parse(line: String) = line.split(",").toSeq.map(_.toLong)

object Solving:
  def solve1(line: String): Long =
    val input = Parsing.parse(line)
    val points =
      for
        x <- 0 until 50
        y <- 0 until 50
      yield Cpu(input).withIn(x, y).allOut
    points.flatten.sum

  def test(x: Long, y: Long)(using input: Seq[Long]) =
    Cpu(input).withIn(x, y).allOut.head == 1L

  def helper(x: Long, y: Long)(using input: Seq[Long]): Long =
    val top  = test(x, y - 99)
    val left = test(x - 99, y)
    if top && left then 10000 * (x - 99) + (y - 99)
    else if top then helper(x + 1, y)
    else helper(x, y + 1)

  def solve2(line: String): Long =
    given input: Seq[Long] = Parsing.parse(line)
    helper(99, 99)

object Main:
  lazy val file = os.pwd / "2019" / "19" / "19.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 169
  println(Main.res2) // part 2: 7001134
