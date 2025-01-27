package aoc2016.day11

object TestStates:
  import DataDefs.*, Level.*, Kind.*, Element.*

  val testState1 = State(
    Map(
      One   -> Box(One, Set(Item(Chip, Lithium))),
      Two   -> Box(Two, Set(Item(RTG, Hydrogen))),
      Three -> Box(Three, Set(Item(RTG, Lithium))),
      Four  -> Box(Four, Set())
    ),
    Box(One, Set(Item(Chip, Hydrogen))) // elevator
  )

  val testState2 = State(
    Map(
      One   -> Box(One, Set(Item(Chip, Hydrogen))),
      Two   -> Box(Two, Set(Item(RTG, Hydrogen))),
      Three -> Box(Three, Set(Item(RTG, Lithium))),
      Four  -> Box(Four, Set())
    ),
    Box(One, Set(Item(Chip, Lithium))) // elevator
  )

  val testState3 = State(
    Map(
      One   -> Box(One, Set()),
      Two   -> Box(Two, Set(Item(RTG, Hydrogen))),
      Three -> Box(Three, Set(Item(RTG, Lithium))),
      Four  -> Box(Four, Set())
    ),
    Box(One, Set(Item(Chip, Hydrogen), Item(Chip, Lithium))) // elevator
  )

  val testStates = Set(testState1, testState2, testState3)

object MainStates:
  import DataDefs.*, Level.*, Kind.*, Element.*

  val mainState1 = State(
    Map(
      One -> Box(One, Set(Item(Chip, Promethium))),
      Two -> Box(
        Two,
        Set(
          Item(RTG, Cobalt),
          Item(RTG, Curium),
          Item(RTG, Ruthenium),
          Item(RTG, Plutonium)
        )
      ),
      Three -> Box(
        Three,
        Set(
          Item(Chip, Cobalt),
          Item(Chip, Curium),
          Item(Chip, Ruthenium),
          Item(Chip, Plutonium)
        )
      ),
      Four -> Box(Four, Set())
    ),
    Box(One, Set(Item(RTG, Promethium))) // elevator
  )

  val mainState2 = State(
    Map(
      One -> Box(One, Set(Item(RTG, Promethium))),
      Two -> Box(
        Two,
        Set(
          Item(RTG, Cobalt),
          Item(RTG, Curium),
          Item(RTG, Ruthenium),
          Item(RTG, Plutonium)
        )
      ),
      Three -> Box(
        Three,
        Set(
          Item(Chip, Cobalt),
          Item(Chip, Curium),
          Item(Chip, Ruthenium),
          Item(Chip, Plutonium)
        )
      ),
      Four -> Box(Four, Set())
    ),
    Box(One, Set(Item(Chip, Promethium))) // elevator
  )

  val mainState3 = State(
    Map(
      One -> Box(One, Set()),
      Two -> Box(
        Two,
        Set(
          Item(RTG, Cobalt),
          Item(RTG, Curium),
          Item(RTG, Ruthenium),
          Item(RTG, Plutonium)
        )
      ),
      Three -> Box(
        Three,
        Set(
          Item(Chip, Cobalt),
          Item(Chip, Curium),
          Item(Chip, Ruthenium),
          Item(Chip, Plutonium)
        )
      ),
      Four -> Box(Four, Set())
    ),
    Box(One, Set(Item(Chip, Promethium), Item(RTG, Promethium))) // elevator
  )

  val mainStates = Set(mainState1, mainState2, mainState3)
