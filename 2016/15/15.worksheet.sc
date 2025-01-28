object DataDefs:
  case class Disc(id: Int, capacity: Int, position: Int):
    def tick = copy(position = (position + 1) % capacity)

  case class State(discs: Seq[Disc], time: Int = 0, capsule: Int = -1):
    def fallsThrough       = capsule == -1 || discs(capsule).position == 0
    def tickWithoutCapsule = State(discs.map(_.tick), time + 1)
    def tickWithCapsule = State(discs.map(_.tick), time + 1, capsule + 1) // assume falls

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Disc = line match
    case s"Disc #$id has $capacity positions; at time=0, it is at position $position." =>
      Disc(id.toInt, capacity.toInt, position.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def fallsAllTheWay(state: State): Boolean =
    if state.capsule == state.discs.size - 1 then state.fallsThrough
    else state.fallsThrough && fallsAllTheWay(state.tickWithCapsule)

  def solve(lines: Seq[String]) =
    var state = State(Parsing.parse(lines))
    while !fallsAllTheWay(state) do state = state.tickWithoutCapsule
    state.time

object Test:
  lazy val file  = os.pwd / "2016" / "15" / "15.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)
// Test.res // part 1: 5

object Main:
  lazy val file1  = os.pwd / "2016" / "15" / "15.input.1.txt"
  lazy val file2  = os.pwd / "2016" / "15" / "15.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve(lines1)
  lazy val res2   = Solving.solve(lines2)
// Main.res1 // part 1: 121834
// Main.res2 // part 2: 3208099
