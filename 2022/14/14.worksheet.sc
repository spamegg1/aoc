object DataDefs:
  val Start = Sand(500, 0)
  val Moves = Seq((0, 1), (-1, 1), (1, 1))

  case class Sand(x: Int, y: Int):
    def move(dx: Int, dy: Int): Sand = Sand(x + dx, y + dy)

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): (Set[Sand], Int) =
    val cave = lines.toSet
      .flatMap: line =>
        line
          .split("\\D+")
          .map(_.toInt)
          .sliding(4, 2)
          .flatMap:
            case Array(x1, y1, x2, y2) =>
              for
                x <- x1.min(x2) to x1.max(x2)
                y <- y1.min(y2) to y1.max(y2)
              yield Sand(x, y)
    (cave, cave.map(_.y).max + 1)

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def fall(cave: Set[Sand], floor: Int, sand: Sand): Sand =
    val next = Moves.map(sand.move).filterNot(cave.contains)
    if sand.y == floor || next.isEmpty then sand else fall(cave, floor, next.head)

  @annotation.tailrec
  def simulate(cave: Set[Sand], floor: Int, predicate: Sand => Boolean): Int =
    val sand = fall(cave, floor, Start)
    if predicate(sand) then cave.size else simulate(cave + sand, floor, predicate)

  def solve1(lines: Seq[String]) =
    val (rock, floor) = Parsing.parse(lines)
    simulate(rock, floor, _.y == floor) - rock.size

  def solve2(lines: Seq[String]) =
    val (rock, floor) = Parsing.parse(lines)
    simulate(rock, floor, _ == Start) - rock.size + 1

object Test:
  val file  = os.pwd / "2022" / "14" / "14.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 24
// Test.res2 // part 2: 93

object Main:
  val file  = os.pwd / "2022" / "14" / "14.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 618
// Main.res2 // part 2: 26358
