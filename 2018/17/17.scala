package aoc2018.day17

object DataDefs:
  type Pos    = (x: Int, y: Int)
  type Ground = (top: Int, bot: Int, clay: Set[Pos])

  extension (p: Pos)
    def left: Pos  = (p.x - 1, p.y)
    def right: Pos = (p.x + 1, p.y)
    def below: Pos = (p.x, p.y + 1)

  enum Result:
    case Moving, Stopped

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Seq[Pos] = line match
    case s"x=$x, y=$ymin..$ymax" => (ymin.toInt to ymax.toInt).map(y => (x.toInt, y))
    case s"y=$y, x=$xmin..$xmax" => (xmin.toInt to xmax.toInt).map(x => (x, y.toInt))

  def parse(lines: Seq[String]): Ground =
    val clay = lines.flatMap(parseLine).toSet
    val ys   = clay.map(_.y)
    (top = ys.min, bot = ys.max, clay = clay)

object Solving:
  import DataDefs.*, Result.*

  def waterfall(ground: Ground): (Int, Int) =
    val stopped           = collection.mutable.Set[Pos]()
    val moving            = collection.mutable.Set[Pos]()
    def blocked(pos: Pos) = stopped.contains(pos) || ground.clay.contains(pos)

    def helper(pos: Pos): Result =
      if blocked(pos) then Stopped
      else if pos.y > ground.bot || moving.contains(pos) then Moving
      else
        helper(pos.below) match
          case Moving =>
            moving += pos
            Moving
          case Stopped =>
            val left = Iterator
              .iterate(pos)(_.left)
              .dropWhile(next => helper(next.below) == Stopped && !blocked(next.left))
              .next()
            val right = Iterator
              .iterate(pos)(_.right)
              .dropWhile(next => helper(next.below) == Stopped && !blocked(next.right))
              .next()
            val poss = for x <- left.x to right.x yield (x, pos.y)
            if blocked(left.left) && blocked(right.right) then
              stopped ++= poss
              Stopped
            else
              moving ++= poss
              Moving
    helper((500, 0)) // mutates moving and stopped in-place
    (moving.filterInPlace(pos => pos.y >= ground.top).size, stopped.size)

  def solve(lines: Seq[String]) = waterfall(Parsing.parse(lines))

object Test:
  lazy val file  = os.pwd / "2018" / "17" / "17.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

object Main:
  lazy val file  = os.pwd / "2018" / "17" / "17.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

@main
def run: Unit =
  println(Test.res) // (28, 29)      part 1: 28+29=57,         part 2: 29
  println(Main.res) // (6431, 35998) part 1: 6431+35998=42429, part 2: 35998
