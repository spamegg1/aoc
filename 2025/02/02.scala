package aoc2025.day02

import scala.util.matching.Regex

object Solving:
  val regex1 = raw"(\d+)\1".r
  val regex2 = raw"(\d+)\1+".r

  def solve(regex: Regex)(line: String) = line
    .split(",")
    .flatMap: pair =>
      val Array(start, end) = pair.split("-")
      (start.toLong to end.toLong).map(_.toString)
    .collect:
      case pat @ regex(_) => pat
    .map(_.toLong)
    .sum

  val solve1 = solve(regex1)
  val solve2 = solve(regex2)

object Test:
  val file = os.pwd / "2025" / "02" / "02.test.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(line)
  val res2 = Solving.solve2(line)

object Main:
  val file = os.pwd / "2025" / "02" / "02.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(line)
  val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 1227775554
  println(Test.res2) // part 2: 4174379265
  println(Main.res1) // part 1: 55916882972
  println(Main.res2) // part 2: 76169125915
