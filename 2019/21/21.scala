package aoc2019.day21

import aoc2019.day09.DataDefs.*

object DataDefs:
  val WALK = Seq("OR A J", "AND B J", "AND C J", "NOT J J", "AND D J")
  val RUN  = Seq("OR E T", "OR H T", "AND T J")

object Parsing:
  def parse(line: String) = line.split(",").toSeq.map(_.toLong)

object Solving:
  import DataDefs.*

  def survey(code: Seq[Long], script: Seq[String]): Long =
    val input = script.map(_ + "\n").mkString.map(_.toLong)
    Cpu(code).withIn(input*).allOut.last

  def solve1(line: String) = survey(Parsing.parse(line), WALK.appended("WALK"))
  def solve2(line: String) = survey(Parsing.parse(line), WALK ++ RUN.appended("RUN"))

object Main:
  lazy val file = os.pwd / "2019" / "21" / "21.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 19353692
  println(Main.res2) // part 2: 1142048514
