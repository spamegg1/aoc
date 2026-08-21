object Solving:
  def fire(dx: Int, dy: Int, lft: Int, rgt: Int, bot: Int, top: Int) = Iterator
    .iterate((0, 0, dx, dy)): (x, y, dx, dy) =>
      (x + dx, y + dy, (dx - 1).max(0), dy - 1)
    .takeWhile((x, y, _, _) => x <= rgt && y >= bot)
    .exists((x, y, _, _) => x >= lft && y <= top)

  def bruteForce(lft: Int, rgt: Int, bot: Int, top: Int): Seq[Int] =
    for
      dx <- math.sqrt(lft).toInt to rgt
      dy <- bot to -bot
      if fire(dx, dy, lft, rgt, bot, top)
    yield (dy * (dy + 1)) / 2
  end bruteForce

  def solve1(lft: Int, rgt: Int, bot: Int, top: Int) =
    bruteForce(lft, rgt, bot, top).max

  def solve2(lft: Int, rgt: Int, bot: Int, top: Int) =
    bruteForce(lft, rgt, bot, top).size

object Test: // target area: x=20..30, y=-10..-5
  val res1 = Solving.solve1(20, 30, -10, -5)
  val res2 = Solving.solve2(20, 30, -10, -5)
// Test.res1 // part 1: 45
// Test.res2 // part 2: 112

object Main: // target area: x=195..238, y=-93..-67
  val res1 = Solving.solve1(195, 238, -93, -67)
  val res2 = Solving.solve2(195, 238, -93, -67)
// Main.res1 // part 1: 4278
// Main.res2 // part 2: 1994
