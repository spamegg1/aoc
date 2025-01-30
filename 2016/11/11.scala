package aoc2016.day11

object DataDefs:
  type Isotope  = String // plutonium, etc.
  type Device   = String // microchip, generator
  type Thing    = (iso: Isotope, dev: Device)
  type Floor    = Seq[Thing]
  type Counter  = Int
  type Elevator = Int
  type State    = (counter: Counter, elevator: Elevator, floors: Seq[Floor])
  type Record   = (elevator: Elevator, floorFreqs: Seq[Map[Device, Counter]])

  extension (floor: Floor)
    def hasOneDeviceType = floor.map(_.dev).distinct.size < 2
    def isPairedSafely = floor
      .filter(_.dev == "microchip")
      .map(_.iso)
      .forall(isotope => floor.contains((iso = isotope, dev = "generator")))
    def isSafe = floor.hasOneDeviceType || floor.isPairedSafely
    def freq   = floor.map(_.dev).groupMapReduce(identity)(_ => 1)(_ + _)

  extension (floors: Seq[Floor])
    def areDone = floors(0).isEmpty && floors(1).isEmpty && floors(2).isEmpty
    def freqs   = floors.map(_.freq)

  extension (s: State)
    def curr   = s.floors(s.elevator)
    def moves1 = s.curr.toSeq.combinations(1)
    def moves2 = s.curr.toSeq.combinations(2)
    def moves  = s.moves1 ++ s.moves2
    def levels = Seq(s.elevator - 1, s.elevator + 1).filter(_ < 4).filter(0 <= _)
    def states =
      for
        move  <- s.moves
        level <- s.levels
        floor  = s.floors(level)   // next floor we move into
        newCur = s.curr.diff(move) // remove things from current floor
        newFlr = floor.union(move) // add things to next floor
        if newCur.isSafe && newFlr.isSafe
        newFloors = s.floors
          .updated(s.elevator, newCur) // remove things from current floor
          .updated(level, newFlr)      // add things to next floor
      yield (counter = s.counter + 1, elevator = level, floors = newFloors)
    def record = (elevator = s.elevator, floorFreqs = s.floors.freqs)

  object State:
    def apply(floors: Seq[Floor]): State = (counter = 0, elevator = 0, floors = floors)

object Parsing:
  import DataDefs.*

  val regex = """(\w+)(?:-compatible)? (microchip|generator)""".r

  def parseLine(line: String): Floor = regex
    .findAllMatchIn(line)
    .map(mat => (iso = mat.group(1), dev = mat.group(2)))
    .toSeq

  def parse(lines: Seq[String]): Seq[Floor] = lines map parseLine

object Solving:
  import DataDefs.*, util.boundary, boundary.break, collection.mutable.Queue

  def solve(lines: Seq[String]) =
    val floors  = Parsing.parse(lines)
    val start   = State(floors)
    var visited = Set(start.record)
    val queue   = Queue(start)
    var result  = 0

    boundary:
      while queue.nonEmpty do
        val state = queue.dequeue()
        if state.floors.areDone then
          result = state.counter
          break()
        for
          nextState <- state.states
          record = nextState.record
        do
          if !visited.contains(record) then
            visited += record
            queue.enqueue(nextState)
    result

object Test:
  lazy val file  = os.pwd / "2016" / "11" / "11.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = Solving.solve(lines)

object Main:
  lazy val file1  = os.pwd / "2016" / "11" / "11.input.txt"
  lazy val file2  = os.pwd / "2016" / "11" / "11.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve(lines1)
  lazy val res2   = Solving.solve(lines2)

@main
def run: Unit =
  println(Test.res)  // part 1: 11
  println(Main.res1) // part 1: 33
  println(Main.res2) // part 2: 57
