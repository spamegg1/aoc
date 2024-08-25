/*
--- Day 6: Probably a Fire Hazard ---
Because your neighbors keep defeating you in the holiday house decorating
contest year after year, you've decided to deploy one million lights in a
1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you
instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
whether to turn on, turn off, or toggle various inclusive ranges given as
coordinate pairs. Each coordinate pair represents opposite corners of a
rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to
9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by
doing the instructions Santa sent you in order.

For example:
turn on 0,0 through 999,999 would turn on (or leave on) every light.
toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off
the ones that were on, and turning on the ones that were off.
turn off 499,499 through 500,500 would turn off (or leave off) the middle four
lights.

After following the instructions, how many lights are lit?

--- Part Two ---
You just finish implementing your winning light pattern when you realize you
mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each
light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of
those lights by 1.

The phrase turn off actually means that you should decrease the brightness of
those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of
those lights by 2.

What is the total brightness of all lights combined after following Santa's
instructions?

For example:
turn on 0,0 through 0,0 would increase the total brightness by 1.
toggle 0,0 through 999,999 would increase the total brightness by 2000000.
 */
object DataDefs:
  enum Light:
    case On, Off

    def toggle: Light = this match
      case On  => Off
      case Off => On
  import Light.*

  case class Brightness(var level: Int):
    def turnOn: Unit = level += 1
    def turnOff: Unit = level = if level == 0 then 0 else level - 1
    def toggle: Unit = level += 2
    def reset: Unit = level = 0

  enum Action:
    case Toggle, TurnOn, TurnOff
  import Action.*

  case class Pos(x: Int, y: Int)

  case class Instr(action: Action, start: Pos, end: Pos)

  case class Grid(lights: Array[Array[Light]]):
    def followInstr(instr: Instr): Unit =
      for
        x <- instr.start.x to instr.end.x
        y <- instr.start.y to instr.end.y
      do
        instr.action match
          case Toggle  => lights(x)(y) = lights(x)(y).toggle
          case TurnOn  => lights(x)(y) = On
          case TurnOff => lights(x)(y) = Off

    def count: Int = lights.map(row => row.count(_ == On)).sum

    def reset: Unit =
      for
        row <- lights
        col <- 0 until row.size
      do row(col) = Off

  case class Grid2(lights: Array[Array[Brightness]]):
    def followInstr(instr: Instr): Unit =
      for
        x <- instr.start.x to instr.end.x
        y <- instr.start.y to instr.end.y
      do
        instr.action match
          case Toggle  => lights(x)(y).toggle
          case TurnOn  => lights(x)(y).turnOn
          case TurnOff => lights(x)(y).turnOff

    def count: Int = lights.map(row => row.map(_.level).sum).sum

    def reset: Unit =
      for
        row <- lights
        col <- 0 until row.size
      do row(col).reset

object Parsing:
  import DataDefs.*, Action.*

  private def parseInstr(line: String): Instr = line match
    case s"toggle $startX,$startY through $endX,$endY" =>
      Instr(
        Toggle,
        Pos(startX.toInt, startY.toInt),
        Pos(endX.toInt, endY.toInt)
      )
    case s"turn on $startX,$startY through $endX,$endY" =>
      Instr(
        TurnOn,
        Pos(startX.toInt, startY.toInt),
        Pos(endX.toInt, endY.toInt)
      )
    case s"turn off $startX,$startY through $endX,$endY" =>
      Instr(
        TurnOff,
        Pos(startX.toInt, startY.toInt),
        Pos(endX.toInt, endY.toInt)
      )

  def parse(lines: Seq[String]): Seq[Instr] = lines.map(parseInstr)

object Solving:
  import DataDefs.*, Light.*

  private val lights = Grid(Array.fill(1000)(Array.fill(1000)(Off)))
  private val lights2 = Grid2(Array.fill(1000)(Array.fill(1000)(Brightness(0))))

  def solve1(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    lights.reset
    instrs foreach lights.followInstr
    lights.count

  def solve2(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    lights2.reset
    instrs foreach lights2.followInstr
    lights2.count

object Testing:
  private lazy val lines = Seq("turn on 499,499 through 500,500")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 4
// Testing.result2 // part 2: 4

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "06" / "06.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 543903
// Main.result2 // part 2: 14687245
