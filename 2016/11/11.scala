package aoc2016.day11

object DataDefs:
  enum Level:
    case One, Two, Three, Four
    def neighbors = this match
      case One   => Set(Two)
      case Two   => Set(One, Three)
      case Three => Set(Two, Four)
      case Four  => Set(Three)
  import Level.*

  enum Kind:
    case Chip, RTG
  import Kind.*

  enum Element:
    case Promethium, Cobalt, Curium, Ruthenium, Plutonium, Hydrogen, Lithium

  case class Item(kind: Kind, elt: Element)

  case class Box(level: Level, items: Set[Item]): // both elevator and floor are boxes
    def size                         = items.size
    def isOperational                = 1 <= size && size <= 2 // for elevator
    def changeLevel(newLevel: Level) = copy(level = newLevel)
    def take(item: Item)             = copy(items = items + item)
    def drop(item: Item)             = copy(items = items - item)
    def safeToGiveTo(that: Box)      = items.filter(that.take(_).isSafe)
    def safeToDrop                   = items.filter(drop(_).isSafe)

    def chips      = items.filter(_.kind == Chip)
    def generators = items.filter(_.kind == RTG)
    def isSafe: Boolean = generators.isEmpty || chips.forall: chip =>
      generators.exists(_.kind == chip.kind)

    def union(that: Box)    = Box(level, items.union(that.items)) // assume same level
    def isSafeOn(that: Box) = union(that).isSafe

  type Floor    = Box
  type Elevator = Box

  case class State(floors: Map[Level, Floor], elevator: Elevator):
    def currentLevel = elevator.level
    def currentFloor = floors(currentLevel)
    def isWon        = floors.forall((lvl, floor) => lvl == Four || floor.items.isEmpty)

    def nextStates: Set[State] =
      // first look at which chips / generators can be picked from / dropped to floor
      val possibleTakesDrops =
        if elevator.size == 1 then // elevator can carry one more item
          currentFloor.safeToDrop.map: item =>
            (elevator.take(item), currentFloor.drop(item))
        else if elevator.size == 2 then // elevator can drop one item to floor
          elevator
            .safeToGiveTo(currentFloor)
            .map(item => (elevator.drop(item), currentFloor.take(item)))
        else Set()

      val possibleElevatorsFloors = possibleTakesDrops + ((elevator, currentFloor))

      // then consider neighbors of current level
      val neighborFloors = elevator.level.neighbors.map(level => level -> floors(level))
      val possibleMoves =
        for
          (neighborLevel, neighborFloor)   <- neighborFloors
          (nextElevator, nextCurrentFloor) <- possibleElevatorsFloors
          movedElevator = nextElevator.changeLevel(neighborLevel)
          if movedElevator.isSafeOn(neighborFloor)
        yield (movedElevator, nextCurrentFloor)

      possibleMoves.map: (movedElevator, nextCurrentFloor) =>
        State(floors.updated(currentLevel, nextCurrentFloor), movedElevator)

object Solving:
  import DataDefs.*

  def solve1(start: Set[State]) =
    var steps   = 0
    val queue   = collection.mutable.Queue.from(start)
    var visited = Set[State]() // avoid going back and forth forever
    var ress    = List[(Int, State)]()

    while queue.nonEmpty do
      val state = queue.dequeue()
      if state.isWon then ress ::= (steps, state)
      else
        visited += state
        queue.enqueueAll(state.nextStates.filterNot(visited.contains))
      steps += 1
    ress

  def solve2(start: Set[State]) = 0L

object Test:
  lazy val start = TestStates.testStates
  lazy val res1  = Solving.solve1(start) // part 1: 11
  lazy val res2  = Solving.solve2(start)

object Main:
  lazy val start = MainStates.mainStates
  lazy val res1  = Solving.solve1(start) // part 1: ???
  lazy val res2  = Solving.solve2(start)

@main
def run: Unit =
  println(Test.res1) // part 1: 11
  // println(Test.res2) // part 2:
  // println(Main.res1) // part 1: ???
  // println(Main.res2) // part 2: ???
