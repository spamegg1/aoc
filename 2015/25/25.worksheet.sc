object Solving:
  def bij(row: Long, col: Long): Long =
    val diagonal      = row + col - 1
    val endOfDiagonal = diagonal * (diagonal + 1) / 2 // value at (1, diagonal)
    endOfDiagonal - (diagonal - col)

  private val firstCode = 20151125L
  private val factor    = 252533L
  private val divisor   = 33554393L
  private val codes     = collection.mutable.Map(1L -> firstCode)

  private def generate(target: Long): Unit =
    for index <- 2L to target do
      codes.get(index) match
        case None =>
          val prev = codes(index - 1L)
          val next = (prev * factor) % divisor
          codes(index) = next
        case Some(value) => ()

  def solve1(row: Long, col: Long) =
    val target = bij(row, col)
    generate(target)
    codes.get(target)

object Test:
  import Solving.*
  assert(bij(1, 1) == 1 && bij(2, 1) == 2 && bij(1, 2) == 3)
  assert(bij(3, 1) == 4 && bij(2, 2) == 5 && bij(1, 3) == 6)
  assert(bij(6, 1) == 16 && bij(4, 3) == 18 && bij(1, 6) == 21)
  lazy val res1 = solve1(6L, 6L)
// Test.res1 // part 1: 27995004

object Main:
  lazy val res1 = Solving.solve1(2947L, 3029L)
// Main.res1 // part 1: 19980801
