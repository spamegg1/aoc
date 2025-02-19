import collection.mutable.ArrayDeque

object DataDefs:
  type Scores = Map[Int, Long] // elf player -> that elf's score

  extension (deque: ArrayDeque[Long])
    def shiftLeft(by: Int): Unit = // remove from left-end, append to right-end
      for _ <- 1 to by do // assume by > 0
        val first = deque.removeHead()
        deque.append(first)
    def shiftRight(by: Int): Unit = // remove from right-end, prepend to left-end
      for _ <- 1 to by do // assume by > 0
        val last = deque.removeLast()
        deque.prepend(last)
    def shift(by: Int): Unit = if by < 0 then shiftLeft(-by) else shiftRight(by)

  case class State(
      scores: Scores,
      marbles: ArrayDeque[Long],
      curElf: Int,
      players: Int,
      nextMarble: Long
  ):
    def highScore = scores.values.max // part 1

    def handleMarbles: Long = // mutates marbles, returns score
      if nextMarble % 23L == 0 then
        marbles.shift(7)
        val deleted = marbles.removeHead()
        deleted + nextMarble
      else
        marbles.shift(-2)
        marbles.prepend(nextMarble)
        0L

    def next =
      val score = handleMarbles
      State(
        scores = scores.updated(curElf, scores(curElf) + score),
        marbles = marbles,
        curElf = (curElf + 1) % players,
        players = players,
        nextMarble = nextMarble + 1L
      )

  object State:
    def apply(players: Int) = new State(
      scores = Map.from((0 until players).map(p => (p, 0L))),
      marbles = ArrayDeque(0L),
      curElf = 0,
      players = players,
      nextMarble = 1L
    )

object Solving:
  import DataDefs.*

  def solve(players: Int)(lastMarble: Long) =
    var state = State(players)
    while state.nextMarble <= lastMarble do state = state.next
    state.highScore

object Test:
  val pt1 = Seq((9, 25L), (10, 1618L), (13, 7999L), (17, 1104L), (21, 6111L), (30, 5807L))
  lazy val res1 = pt1.map((p, m) => Solving.solve(p)(m))
// Test.res1 // part 1: 32,8317,146373,2764,54718,37305

object Main:
  lazy val res1 = Solving.solve(470)(72170L)
  lazy val res2 = Solving.solve(470)(7217000L)
// Main.res1 // part 1: 388024
// Main.res2 // part 2: 3180929875
