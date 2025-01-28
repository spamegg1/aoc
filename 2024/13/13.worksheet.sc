/*
--- Day 13: Claw Contraption ---
Next up: the lobby of a resort on a tropical island.
The Historians take a moment to admire the hexagonal floor tiles before spreading out.

Fortunately, it looks like the resort has a new arcade!
Maybe you can win some prizes from the claw machines?

The claw machines here are a little unusual.
Instead of a joystick or directional buttons to control the claw,
these machines have two buttons labeled A and B.
Worse, you can't just put in a token and play; it costs 3 tokens
to push the A button and 1 token to push the B button.

With a little experimentation, you figure out that each machine's buttons
are configured to move the claw a specific amount to the right (along the X axis)
and a specific amount forward (along the Y axis) each time that button is pressed.

Each machine contains one prize; to win the prize,
the claw must be positioned exactly above the prize on both the X and Y axes.

You wonder: what is the smallest number of tokens you would have to
spend to win as many prizes as possible? You assemble a list of
every machine's button behavior and prize location (your puzzle input).
For example:

Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

This list describes the button configuration and prize location
of four different claw machines.

For now, consider just the first claw machine in the list:
  Pushing the machine's A button would move the claw 94 units along
    the X axis and 34 units along the Y axis.
  Pushing the B button would move the claw 22 units along the X axis
    and 67 units along the Y axis.
  The prize is located at X=8400, Y=5400; this means that from the
    claw's initial position, it would need to move exactly 8400 units
    along the X axis and exactly 5400 units along the Y axis to be
    perfectly aligned with the prize in this machine.

The cheapest way to win the prize is by pushing the A button 80 times
and the B button 40 times. This would line up the claw along the X axis
(because 80*94 + 40*22 = 8400) and along the Y axis (because 80*34 + 40*67 = 5400).
Doing this would cost 80*3 tokens for the A presses and 40*1 for the B presses,
a total of 280 tokens.

For the second and fourth claw machines, there is no
combination of A and B presses that will ever win a prize.

For the third claw machine, the cheapest way to win the prize is
by pushing the A button 38 times and the B button 86 times.
Doing this would cost a total of 200 tokens.

So, the most prizes you could possibly win is two;
the minimum tokens you would have to spend to win all (two) prizes is 480.

You estimate that each button would need to be pressed no more than 100 times
to win a prize. How else would someone be expected to play?

Figure out how to win as many prizes as possible.
What is the fewest tokens you would have to spend to win all possible prizes?

--- Part Two ---
As you go to win the first prize, you discover that the claw is
nowhere near where you expected it would be. Due to a unit conversion
error in your measurements, the position of every prize is actually
10000000000000 higher on both the X and Y axis!

Add 10000000000000 to the X and Y position of every prize.
After making this change, the example above would now look like this:

Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=10000000008400, Y=10000000005400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=10000000012748, Y=10000000012176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=10000000007870, Y=10000000006450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=10000000018641, Y=10000000010279

Now, it is only possible to win a prize on the second and fourth claw machines.
Unfortunately, it will take many more than 100 presses to do so.

Using the corrected prize coordinates, figure out how to win as many prizes as possible.
What is the fewest tokens you would have to spend to win all possible prizes?
 */
object DataDefs:
  val ACOST = 3L
  val BCOST = 1L
  val LIMIT = 100L
  val ERROR = 10000000000000L

  type Button  = String
  type Claw    = (button: Button, x: Long, y: Long)
  type Prize   = (x: Long, y: Long)
  type Machine = (a: Claw, b: Claw, prize: Prize)
  type Problem = (a: Long, b: Long, target: Long)

  extension (numer: Long)
    def toLongOption(denom: Long) =
      Option.when(denom != 0 && numer % denom == 0)(numer / denom)

  extension (p: Problem)
    def findCombos = // part 1: naive brute force to find linear combination
      for
        x <- 1L to math.min(p.target / p.a, LIMIT)
        y <- 1L to math.min(p.target / p.b, LIMIT)
        if p.a * x + p.b * y == p.target
      yield (x, y)

    def solve(that: Problem): Option[(Long, Long)] = // part 2
      val (ax, bx, p1) = p    // assume "this" is the x problem, "that" is y
      val (ay, by, p2) = that // assume exactly 1 solution exists
      val (px, py)     = (p1 + ERROR, p2 + ERROR)
      val bNumer       = py * ax - px * ay
      val bDenom       = by * ax - bx * ay
      val bOption      = bNumer.toLongOption(bDenom)
      val aOption      = bOption.flatMap(b => (px - bx * b).toLongOption(ax))
      for a <- aOption; b <- bOption yield (a, b)

  extension (m: Machine)
    def xProb     = (a = m.a.x, b = m.b.x, target = m.prize.x)       // both parts
    def yProb     = (a = m.a.y, b = m.b.y, target = m.prize.y)       // both parts
    def xSols     = m.xProb.findCombos                               // part 1
    def ySols     = m.yProb.findCombos                               // part 1
    def solutions = m.xSols intersect m.ySols                        // part 1
    def costs     = m.solutions.map((a, b) => a * ACOST + b * BCOST) // part 1
    def minCost   = m.costs.minOption.getOrElse(0L)                  // part 1
    def solve2    = m.xProb.solve(m.yProb)                           // part 2
    def cost2     = m.solve2.map((a, b) => a * ACOST + b * BCOST).getOrElse(0L)

object Parsing:
  import DataDefs.*

  def parseClaw(line: String): Claw = line match
    case s"Button $b: X+$x, Y+$y" => (button = b, x = x.toLong, y = y.toLong)

  def parsePrize(line: String): Prize = line match
    case s"Prize: X=$x, Y=$y" => (x = x.toLong, y = y.toLong)

  def parseMachine(lines: String): Machine =
    val Seq(a, b, prize) = lines.split("\n").toSeq
    (a = parseClaw(a), b = parseClaw(b), prize = parsePrize(prize))

  def parse(lines: String): Seq[Machine] = lines.split("\n\n").toSeq.map(parseMachine)

object Solving:
  import DataDefs.*

  def solve1(lines: String) = Parsing
    .parse(lines)
    .map(_.minCost)
    .sum

  def solve2(lines: String) = Parsing
    .parse(lines)
    .map(_.cost2)
    .sum

object Test:
  lazy val lines   = os.read(os.pwd / "2024" / "13" / "13.test.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Test.res1 // part 1: 480
// Test.res2 // part 2: 875318608908

object Main:
  lazy val lines   = os.read(os.pwd / "2024" / "13" / "13.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 32041
// Main.res2 // part 2: 95843948914827
