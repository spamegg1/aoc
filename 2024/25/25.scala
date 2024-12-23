/*

 */
package aoc2024.day25

object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]) = 0L

object Testing:
  lazy val lines  = os.read.lines(os.pwd / "2024" / "25" / "25.test.input.txt")
  lazy val result = Solving.solve(lines)
// Testing.result // part 1:

object Main:
  lazy val lines  = os.read.lines(os.pwd / "2024" / "25" / "25.input.txt")
  lazy val result = Solving.solve(lines)
// Main.result // part 1:
