object DataDefs:
  enum Light:
    case On, Off
    def toggle: Light = this match
      case On  => Off
      case Off => On
  import Light.*

  case class Brightness(var level: Int):
    def turnOn: Unit  = level += 1
    def turnOff: Unit = level = if level == 0 then 0 else level - 1
    def toggle: Unit  = level += 2
    def reset: Unit   = level = 0

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
      Instr(Toggle, Pos(startX.toInt, startY.toInt), Pos(endX.toInt, endY.toInt))
    case s"turn on $startX,$startY through $endX,$endY" =>
      Instr(TurnOn, Pos(startX.toInt, startY.toInt), Pos(endX.toInt, endY.toInt))
    case s"turn off $startX,$startY through $endX,$endY" =>
      Instr(TurnOff, Pos(startX.toInt, startY.toInt), Pos(endX.toInt, endY.toInt))

  def parse(lines: Seq[String]): Seq[Instr] = lines.map(parseInstr)

object Solving:
  import DataDefs.*, Light.*

  private val lights1 = Grid(Array.fill(1000)(Array.fill(1000)(Off)))
  private val lights2 = Grid2(Array.fill(1000)(Array.fill(1000)(Brightness(0))))

  def solve1(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    lights1.reset
    instrs foreach lights1.followInstr
    lights1.count

  def solve2(lines: Seq[String]) =
    val instrs = Parsing.parse(lines)
    lights2.reset
    instrs foreach lights2.followInstr
    lights2.count

object Test:
  lazy val lines = Seq("turn on 499,499 through 500,500")
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 4

object Main:
  lazy val file  = os.pwd / "2015" / "06" / "06.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 543903
// Main.res2 // part 2: 14687245
