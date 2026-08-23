object DataDefs:
  import collection.mutable.{Map => MMap}
  type Cache = MMap[State, Total]

  val Threshold  = 1000
  val ScoreLimit = 21
  val Dirac: Seq[Int] =
    for
      first  <- 1 to 3
      second <- 1 to 3
      third  <- 1 to 3
    yield first + second + third

  case class Player(space: Int, score: Int = 0):
    def next(move: Int) =
      val nextSpace = (((space - 1) + move) % 10) + 1
      Player(nextSpace, score + nextSpace)
    def partition = Dirac.map(next).partition(_.score >= ScoreLimit)

  case class State(p1: Player, p2: Player)

  case class Dice(value: Int = 1, rolled: Int = 0):
    def roll = Dice(value % 100 + 1, rolled + 1)
    def thrice: (Dice, Int) = Iterator
      .iterate((this, 0))((dice, move) => (dice.roll, move + dice.value))
      .drop(3)
      .next()

  case class Total(p1Win: Long, p2Win: Long):
    def +(other: Total) = Total(p1Win + other.p1Win, p2Win + other.p2Win)
    def max: Long       = p1Win.max(p2Win)
end DataDefs

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def play(state: State, dice: Dice): Int =
    val State(p1, p2)   = state
    val (dice1, p1Move) = dice.thrice
    val nextPlayer1     = p1.next(p1Move)
    val (dice2, p2Move) = dice1.thrice
    val nextPlayer2     = p2.next(p2Move)
    if nextPlayer1.score >= Threshold then p2.score * dice1.rolled
    else if nextPlayer2.score >= Threshold then nextPlayer1.score * dice2.rolled
    else play(State(nextPlayer1, nextPlayer2), dice2)
  end play

  def checkCache(state: State)(using cache: Cache): Total =
    cache.getOrElseUpdate(state, compute(state))

  def compute(state: State)(using cache: Cache): Total =
    val (winsP1, rest) = state.p1.partition
    rest
      .map: nextP1 =>
        val (winsP2, other) = state.p2.partition
        other
          .map(nextP2 => checkCache(State(nextP1, nextP2)))
          .fold(Total(0, winsP2.size))(_ + _)
      .fold(Total(winsP1.size, 0))(_ + _)
  end compute

  def solve1(p1: Int, p2: Int) = play(State(Player(p1), Player(p2)), Dice())
  def solve2(p1: Int, p2: Int) =
    given Cache = collection.mutable.Map[State, Total]()
    checkCache(State(Player(p1), Player(p2))).max
end Solving

object Test: // Player 1 starting position: 4, Player 2 starting position: 8
  val res1 = Solving.solve1(4, 8)
  val res2 = Solving.solve2(4, 8)
// Test.res1 // part 1: 739785
// Test.res2 // part 2: res1: Long = 444356092776315

object Main: // Player 1 starting position: 3, Player 2 starting position: 10
  val res1 = Solving.solve1(3, 10)
  val res2 = Solving.solve2(3, 10)
// Main.res1 // part 1: 713328
// Main.res2 // part 2: 92399285032143
