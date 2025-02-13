/*
--- Day 19: Tractor Beam ---
Unsure of the state of Santa's ship, you borrowed the tractor beam
technology from Triton. Time to test it out.

When you're safely away from anything else, you activate the tractor beam, but nothing
happens. It's hard to tell whether it's working if there's nothing to use it on.
Fortunately, your ship's drone system can be configured to deploy a drone to
specific coordinates and then check whether it's being pulled.
There's even an Intcode program (your puzzle input) that gives you
access to the drone system.

The program uses two input instructions to request the X and Y position
to which the drone should be deployed. Negative numbers are invalid and
will confuse the drone; all numbers should be zero or positive.

Then, the program will output whether the drone is stationary (0) or being
pulled by something (1). For example, the coordinate X=0, Y=0 is directly
in front of the tractor beam emitter, so the drone control program will
always report 1 at that location.

To better understand the tractor beam, it is important to get a good picture
of the beam itself. For example, suppose you scan the 10x10 grid of points
closest to the emitter:

       X
  0->      9
 0#.........
 |.#........
 v..##......
  ...###....
  ....###...
Y .....####.
  ......####
  ......####
  .......###
 9........##

In this example, the number of points affected by the
tractor beam in the 10x10 area closest to the emitter is 27.

However, you'll need to scan a larger area to understand the shape of the beam.
How many points are affected by the tractor beam in the 50x50 area closest to
the emitter? (For each of X and Y, this will be 0 through 49.)

 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parseLine(line: String) = ???
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(line: String) = 0L
  def solve2(line: String) = 0L

object Test:
  private lazy val lines = os.read.lines(os.pwd / "2019" / "19" / "01.test.input.txt")
  lazy val res1 = lines map Solving.solve1
  lazy val res2 = lines map Solving.solve2
// Test.res1 // part 1: 27
// Test.res2 // part 2:

object Main:
  lazy val line = os.read.lines(os.pwd / "2019" / "19" / "19.input.txt").head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1:
// Main.res2 // part 2:
