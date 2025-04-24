import scala.util.boundary, boundary.break

object DataDefs:
  enum State:
    case Floor, Empty, Occupied
  import State.*

  type Stepper = (Int, Int) => (Int, Int)
  type Pred    = (Int, Int) => Seq[State]

  case class Seats(seats: Seq[Seq[State]], width: Int, height: Int):
    private def inbounds(r: Int, c: Int) = // part 1
      0 <= r && r < height && 0 <= c && c < width

    private def neighbors(row: Int, col: Int) = Seq( // part 1
      (row - 1, col - 1), // row above
      (row - 1, col),
      (row - 1, col + 1),
      (row, col - 1), // row current
      (row, col + 1),
      (row + 1, col - 1), // row below
      (row + 1, col),
      (row + 1, col + 1)
    ).filter(inbounds)

    private def adjacent(row: Int, col: Int): Seq[State] = // part 1
      neighbors(row, col).map((r, c) => seats(r)(c))

    // good example of mixing higher order functions + flatMap + local mutable state
    private def goInDirection(row: Int, col: Int)(fun: Stepper) =
      var res: Option[State] = None
      var next               = fun(row, col)
      boundary:
        while inbounds.tupled(next) do
          val lookup = seats(next._1)(next._2)
          if lookup != Floor then
            res = Some(lookup)
            break()
          else next = fun.tupled(next)
      res

    private def visible(row: Int, col: Int): Seq[State] = // part 2
      Seats.allDirs.flatMap(goInDirection(row, col))

    private def check(current: State, states: Seq[State], count: Int) = // both
      current match
        case Floor    => Floor
        case Empty    => if states.forall(_ != Occupied) then Occupied else Empty
        case Occupied => if states.count(_ == Occupied) >= count then Empty else Occupied

    private def nextState(pred: Pred, count: Int)(row: Int, col: Int) =
      check(seats(row)(col), pred(row, col), count)

    private val nextState1 = nextState(adjacent, 4) // part 1
    private val nextState2 = nextState(visible, 5)  // part 2

    private def nextSeats(stateFun: (Int, Int) => State) = // both parts
      for row <- 0 until height
      yield for col <- 0 until width
      yield stateFun(row, col)

    private lazy val nextSeats1 = nextSeats(nextState1)            // part 1
    lazy val next1              = Seats(nextSeats1, width, height) // part 1
    lazy val stabilized1        = nextSeats1 == seats              // part 1

    private lazy val nextSeats2 = nextSeats(nextState2)            // part 2
    lazy val next2              = Seats(nextSeats2, width, height) // part 2
    lazy val stabilized2        = nextSeats2 == seats              // part 2

    lazy val totalOccupied = seats.map(_.count(_ == Occupied)).sum // both parts

  object Seats:
    val n       = (row: Int, col: Int) => (row - 1, col)
    val s       = (row: Int, col: Int) => (row + 1, col)
    val e       = (row: Int, col: Int) => (row, col + 1)
    val w       = (row: Int, col: Int) => (row, col - 1)
    val ne      = (row: Int, col: Int) => (row - 1, col + 1)
    val nw      = (row: Int, col: Int) => (row - 1, col - 1)
    val se      = (row: Int, col: Int) => (row + 1, col + 1)
    val sw      = (row: Int, col: Int) => (row + 1, col - 1)
    val allDirs = Seq(n, s, e, w, ne, nw, se, sw)

object Parsing:
  import DataDefs.State.*

  extension (c: Char)
    def toState = c match
      case 'L' => Empty
      case '#' => Occupied
      case _   => Floor

  def parseLine(line: String)   = line map (_.toState)
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    var seats = Seats(Parsing.parse(lines), lines.head.size, lines.size)
    while !seats.stabilized1 do seats = seats.next1
    seats.totalOccupied

  def solve2(lines: Seq[String]) =
    var seats = Seats(Parsing.parse(lines), lines.head.size, lines.size)
    while !seats.stabilized2 do seats = seats.next2
    seats.totalOccupied

object Test:
  lazy val file  = os.pwd / "2020" / "11" / "11.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 37
// Test.res2 // part 2: 26

object Main:
  lazy val file  = os.pwd / "2020" / "11" / "11.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 2270
// Main.res2 // part 2: 2042
