object DataDefs:
  case class Light(px: Int, py: Int, vx: Int, vy: Int):
    def move(seconds: Int) = Light(px + vx * seconds, py + vy * seconds, vx, vy)

  case class Grid(lights: Seq[Light]):
    val minHeight = lights.minBy(_.py).py
    val minWidth  = lights.minBy(_.px).px
    val maxHeight = lights.maxBy(_.py).py + 1
    val maxWidth  = lights.maxBy(_.px).px + 1
    val array = Array.fill(maxHeight - minHeight)(Array.fill(maxWidth - minWidth)(' '))
    val grid =
      for light <- lights do array(light.py - minHeight)(light.px - minWidth) = '#'
      array
    val show: String = grid.map(_.mkString).mkString("\n")

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line match
    case s"position=<$px,$py>velocity=<$vx,$vy>" =>
      Light(px.toInt, py.toInt, vx.toInt, vy.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String]): Unit =
    val lights    = Parsing.parse(lines)
    val guess     = 10595 // 10000 is obvious from input, rest is guesswork
    val newLights = lights map (_.move(guess))
    val grid      = Grid(newLights)
    // os.write(os.pwd / "2018" / "10" / s"${guess}.txt", grid.show)

object Main:
  lazy val file  = os.pwd / "2018" / "10" / "10.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines) // check output text file
// Main.res // part 1: JLPZFJRH, part 2: 10595
