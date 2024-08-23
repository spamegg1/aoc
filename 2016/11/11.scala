/*
--- Day 11: Radioisotope Thermoelectric Generators ---
You come upon a column of four floors that have been entirely sealed
off from the rest of the building except for a small dedicated lobby.
There are some radiation warnings and a big sign which reads
"Radioisotope Testing Facility".

According to the project status board, this facility is currently
being used to experiment with Radioisotope Thermoelectric Generators
(RTGs, or simply "generators") that are designed to be paired with
specially-constructed microchips. Basically, an RTG is a highly
radioactive rock that generates electricity through heat.

The experimental RTGs have poor radiation containment, so they're
dangerously radioactive. The chips are prototypes and don't have
normal radiation shielding, but they do have the ability to generate
an electromagnetic radiation shield when powered.
Unfortunately, they can only be powered by their corresponding RTG.
An RTG powering a microchip is still dangerous to other microchips.

In other words, if a chip is ever left in the same area as another RTG,
and it's not connected to its own RTG, the chip will be fried.
Therefore, it is assumed that you will follow procedure and keep chips
connected to their corresponding RTG when they're in the same room,
and away from other RTGs otherwise.

These microchips sound very interesting and useful to your current
activities, and you'd like to try to retrieve them.
The fourth floor of the facility has an assembling machine which can
make a self-contained, shielded computer for you to take with you -
that is, if you can bring it all of the RTGs and microchips.

Within the radiation-shielded part of the facility
(in which it's safe to have these pre-assembly RTGs),
there is an elevator that can move between the four floors.
Its capacity rating means it can carry at most yourself and
two RTGs or microchips in any combination.
(They're rigged to some heavy diagnostic equipment -
the assembling machine will detach it for you.)
As a security measure, the elevator will only function if it
contains at least one RTG or microchip.
The elevator always stops on each floor to recharge,
and this takes long enough that the items within it and
the items on that floor can irradiate each other.
(You can prevent this if a Microchip and its Generator
end up on the same floor in this way, as they can be
connected while the elevator is recharging.)

You make some notes of the locations of each component of interest
(your puzzle input). Before you don a hazmat suit and start moving
things around, you'd like to have an idea of what you need to do.

When you enter the containment area, you and the elevator will start on the first floor.

For example, suppose the isolated area has the following arrangement:

The first floor contains a hydrogen-compatible
microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium,
M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .
F3 .  .  .  LG .
F2 .  HG .  .  .
F1 E  .  HM .  LM

Then, to get everything up to the assembling machine on the fourth floor,
the following steps could be taken:
  Bring the Hydrogen-compatible Microchip to the second floor,
  which is safe because it can get power from the Hydrogen Generator:
    F4 .  .  .  .  .
    F3 .  .  .  LG .
    F2 E  HG HM .  .
    F1 .  .  .  .  LM

  Bring both Hydrogen-related items to the third floor,
  which is safe because the Hydrogen-compatible microchip
  is getting power from its generator:

    F4 .  .  .  .  .
    F3 E  HG HM LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  LM

  Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible
  Microchip back down with you so you can still use the elevator:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 E  .  HM .  .
    F1 .  .  .  .  LM

  At the first floor, grab the Lithium-compatible Microchip,
  which is safe because Microchips don't affect each other:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 .  .  .  .  .
    F1 E  .  HM .  LM

  Bring both Microchips up one floor, where there is nothing to fry them:

    F4 .  .  .  .  .
    F3 .  HG .  LG .
    F2 E  .  HM .  LM
    F1 .  .  .  .  .

  Bring both Microchips up again to floor three, where they can be
  temporarily connected to their corresponding generators while the
  elevator recharges, preventing either of them from being fried:

    F4 .  .  .  .  .
    F3 E  HG HM LG LM
    F2 .  .  .  .  .
    F1 .  .  .  .  .

  Bring both Microchips to the fourth floor:

    F4 E  .  HM .  LM
    F3 .  HG .  LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

  Leave the Lithium-compatible microchip on the fourth floor,
  but bring the Hydrogen-compatible one so you can still use the elevator;
  this is safe because although the Lithium Generator is on the destination floor,
  you can connect Hydrogen-compatible microchip to the Hydrogen Generator there:

    F4 .  .  .  .  LM
    F3 E  HG HM LG .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

  Bring both Generators up to the fourth floor, which is safe because you can
  connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:

    F4 E  HG .  LG LM
    F3 .  .  HM .  .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

  Bring the Lithium Microchip with you to the third floor so you can use the elevator:

    F4 .  HG .  LG .
    F3 E  .  HM .  LM
    F2 .  .  .  .  .
    F1 .  .  .  .  .

  Bring both Microchips to the fourth floor:

    F4 E  HG HM LG LM
    F3 .  .  .  .  .
    F2 .  .  .  .  .
    F1 .  .  .  .  .

In this arrangement, it takes 11 steps to collect all of the objects
at the fourth floor for assembly.
(Each elevator stop counts as one step, even if nothing is added to or removed from it.)

In your situation, what is the minimum number of steps
required to bring all of the objects to the fourth floor?

 */
package day11

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
    def size = items.size
    def isOperational = 1 <= size && size <= 2 // for elevator
    def changeLevel(newLevel: Level) = copy(level = newLevel)
    def take(item: Item) = copy(items = items + item)
    def drop(item: Item) = copy(items = items - item)
    def safeToGiveTo(that: Box) = items.filter(that.take(_).isSafe)
    def safeToDrop = items.filter(drop(_).isSafe)

    def chips = items.filter(_.kind == Chip)
    def generators = items.filter(_.kind == RTG)
    def isSafe: Boolean = generators.isEmpty || chips.forall: chip =>
      generators.exists(_.kind == chip.kind)

    def union(that: Box) = Box(level, items.union(that.items)) // assume same level
    def isSafeOn(that: Box) = union(that).isSafe

  type Floor = Box
  type Elevator = Box

  case class State(floors: Map[Level, Floor], elevator: Elevator):
    def currentLevel = elevator.level
    def currentFloor = floors(currentLevel)
    def isWon = floors.forall((lvl, floor) => lvl == Four || floor.items.isEmpty)

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
          (neighborLevel, neighborFloor) <- neighborFloors
          (nextElevator, nextCurrentFloor) <- possibleElevatorsFloors
          movedElevator = nextElevator.changeLevel(neighborLevel)
          if movedElevator.isSafeOn(neighborFloor)
        yield (movedElevator, nextCurrentFloor)

      possibleMoves.map: (movedElevator, nextCurrentFloor) =>
        State(floors.updated(currentLevel, nextCurrentFloor), movedElevator)

object Solving:
  import DataDefs.*

  def solve1(start: Set[State]) =
    var steps = 0
    val queue = collection.mutable.Queue.from(start)
    var visited = Set[State]() // avoid going back and forth forever
    var results = List[(Int, State)]()

    while queue.nonEmpty do
      val state = queue.dequeue()
      if state.isWon then results ::= (steps, state)
      else
        visited += state
        queue.enqueueAll(state.nextStates.filterNot(visited.contains))
      steps += 1
    results

  def solve2(start: Set[State]) = 0L

object Testing:
  private lazy val start = TestStates.testStates
  lazy val result1 = Solving.solve1(start) // part 1: 11
  lazy val result2 = Solving.solve2(start)

object Main:
  private lazy val start = MainStates.mainStates
  lazy val result1 = Solving.solve1(start) // part 1: ???
  lazy val result2 = Solving.solve2(start)

@main
def run: Unit =
  println(Testing.result1) // part 1: 11
  // println(Testing.result2) // part 2:
  // println(Main.result1) // part 1: ???
  // println(Main.result2) // part 2: ???
