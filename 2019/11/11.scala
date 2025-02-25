package aoc2019.day11

import aoc2019.day09.DataDefs.{Cpu, State}

object DataDefs:
  type Pos    = (x: Int, y: Int)
  type Panels = Map[Pos, Long]

  extension (p: Pos)
    def +(that: Pos) = (p.x + that.x, p.y + that.y)
    def cw: Pos      = (-p.y, p.x)
    def ccw: Pos     = (p.y, -p.x)

  extension (panels: Panels)
    def minX = panels.keys.map(_.x).min
    def maxX = panels.keys.map(_.x).max
    def minY = panels.keys.map(_.y).min
    def maxY = panels.keys.map(_.y).max
    def show: String =
      (for y <- minY to maxY
      yield (for x <- minX to maxX
      yield if panels((x, y)) == 1 then '#' else ' ').mkString).mkString("\n")

object Parsing:
  def parse(line: String) = line.split(",").map(_.toLong).toSeq

object Solving:
  import DataDefs.*, State.*

  @annotation.tailrec
  def paint(cpu: Cpu, dir: Pos, pos: Pos, panels: Panels): Panels =
    val first = cpu.withIn(panels(pos)).nextOut
    first.state match
      case Out(color) =>
        val second  = first.nextOut
        val turn    = second.state.out
        val nextDir = if turn == 1 then dir.cw else dir.ccw
        paint(second, nextDir, pos + nextDir, panels.updated(pos, color))
      case _ => panels

  def solve1(line: String) =
    val mem    = Parsing.parse(line)
    val panels = Map[Pos, Long]().withDefaultValue(0L)
    paint(Cpu(mem), (0, -1), (0, 0), panels).size

  def solve2(line: String) =
    val mem    = Parsing.parse(line)
    val start  = Map((x = 0, y = 0) -> 1L).withDefaultValue(0L)
    val panels = paint(Cpu(mem), (0, -1), (0, 0), start)
    panels.show

object Main:
  lazy val file = os.pwd / "2019" / "11" / "11.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 2141
  println(Main.res2) // part 2: RPJCFZKF
