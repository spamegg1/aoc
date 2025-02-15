object DataDefs:
  // part 1, this is too slow for part 2. We could reuse part2 but keep this for education
  case class Spinlock(cur: Int, buffer: Vector[Int], size: Int, stepSize: Int):
    private lazy val insertAt     = (cur + stepSize) % size + 1
    private lazy val (head, tail) = (buffer.take(insertAt), buffer.drop(insertAt))
    private lazy val newBuffer    = head ++ Vector(size) ++ tail
    lazy val next                 = Spinlock(insertAt, newBuffer, size + 1, stepSize)
    lazy val lastValue            = buffer(cur + 1)

  // part 2, faster to deal with 50,000,000 insertions. Only track nextToZero.
  case class Spinfast(cur: Int, size: Int, stepSize: Int, nextToZero: Int):
    private lazy val insertAt      = (cur + stepSize) % size + 1
    private lazy val newNextToZero = if insertAt == 1 then size else nextToZero
    lazy val next                  = Spinfast(insertAt, size + 1, stepSize, newNextToZero)

object Solving:
  import DataDefs.*

  def solve1(stepSize: Int)(numSteps: Int) =
    var spin = Spinlock(0, Vector(0), 1, stepSize) // start at (0) state
    for _ <- 0 until numSteps do spin = spin.next
    spin.lastValue

  def solve2(stepSize: Int)(numSteps: Int) =
    var spin = Spinfast(1, 2, stepSize, 1) // start at 0 (1) state
    for _ <- 0 until numSteps do spin = spin.next
    spin.nextToZero

object Test:
  lazy val res1 = Solving.solve1(3)(2017)
  lazy val res2 = Solving.solve2(3)(2017)
// Test.res1 // part 1: 638
// Test.res2 // part 2: 1226

object Main:
  lazy val res1 = Solving.solve1(316)(2017)
  lazy val res2 = Solving.solve2(316)(50_000_000)
// Main.res1 // part 1: 180
// Main.res2 // part 2: 13326437
