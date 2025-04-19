package aoc2019.day17

import aoc2019.day09.DataDefs.*

object DataDefs:
  type Pos = (x: Int, y: Int)
  extension (p: Pos) def +(that: Pos): Pos = (p.x + that.x, p.y + that.y)
  object Pos:
    val neighbors: Seq[Pos] = Seq((0, -1), (0, 1), (-1, 0), (1, 0))

object Parsing:
  def parse(line: String) = line.split(",").toSeq.map(_.toLong)

object Solving:
  import DataDefs.*

  def solve1(line: String) =
    val cpu = Cpu(Parsing.parse(line))
    val out: Seq[String] = cpu
      .allOut
      .map(_.toChar)
      .mkString
      .split("\n")
      .toSeq

    val posns: Seq[Pos] =
      for
        y <- 0 until out.size
        x <- 0 until out.head.size
        if out(y)(x) == '#'
      yield (x = x, y = y)

    posns.foldLeft(0L): (total, pos) =>
      if Pos.neighbors.map(pos + _).forall(posns.contains)
      then total + pos.x * pos.y
      else total

  def printPath(line: String) = // result is saved in 17.txt
    val cpu = Cpu(Parsing.parse(line))
    val out = cpu
      .allOut
      .map(_.toChar)
      .mkString
    println(out)

  // Based on the path in 17.txt, written manually:
  val fullPath   = "R,6,L,12,R,6,R,6,L,12,R,6,L,12,R,6,L,8,L,12,R,12,L,10,L,10,L,12,R,6,L,8,L,12,R,12,L,10,L,10,L,12,R,6,L,8,L,12,R,12,L,10,L,10,L,12,R,6,L,8,L,12,R,6,L,12,R,6"
  val mainRoutine = "A,A,B,C,B,C,B,C,B,A"
  val functionA   = "R,6,L,12,R,6" // figured out these by looking at repeating patterns.
  val functionB   = "L,12,R,6,L,8,L,12"
  val functionC   = "R,12,L,10,L,10"
  val videoFeed   = "n" // no, we don't want video feed
  val movements = Seq(mainRoutine, functionA, functionB, functionC, videoFeed)

  def solve2(line: String) =
    val cpu = Cpu(Parsing.parse(line).updated(0, 2L))
    val input = movements.flatMap(_ + "\n").map(_.toLong) // convert to ASCII
    cpu.withIn(input*).allOut.last

object Main:
  lazy val file = os.pwd / "2019" / "17" / "17.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 13580
  println(Main.res2) // part 2: 1063081
