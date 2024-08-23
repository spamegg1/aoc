/*
--- Day 13: Packet Scanners ---
You need to cross a vast firewall. The firewall consists of several layers,
each with a security scanner that moves back and forth across the layer.
To succeed, you must not be detected by a scanner.

By studying the firewall briefly, you are able to record (in your puzzle input)
the depth of each layer and the range of the scanning area for the scanner within it,
written as depth: range. Each layer has a thickness of exactly 1.
A layer at depth 0 begins immediately inside the firewall;
a layer at depth 1 would start immediately after that.

For example, suppose you've recorded the following:
0: 3
1: 2
4: 4
6: 4
This means that there is a layer immediately inside the firewall (with range 3),
a second layer immediately after that (with range 2),
a third layer which begins at depth 4 (with range 4),
and a fourth layer which begins at depth 6 (also with range 4).
Visually, it might look like this:
 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

Within each layer, a security scanner moves back and forth within its range.
Each security scanner starts at the top and moves down until it reaches the bottom,
then moves up until it reaches the top, and repeats.
A security scanner takes one picosecond to move one step.
Drawing scanners as S, the first few picoseconds look like this:

Picosecond 0:
 0   1   2   3   4   5   6
[S] [S] ... ... [S] ... [S]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 1:
 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 2:
 0   1   2   3   4   5   6
[ ] [S] ... ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

Picosecond 3:
 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] ... [ ]
[S] [S]         [ ]     [ ]
[ ]             [ ]     [ ]
                [S]     [S]

Your plan is to hitch a ride on a packet about to move through the firewall.
The packet will travel along the top of each layer,
and it moves at one layer per picosecond.
Each picosecond, the packet moves one layer forward
(its first move takes it into layer 0), and then the scanners move one step.
If there is a scanner at the top of the layer as your packet enters it, you are caught.
(If a scanner moves into the top of its layer while you are there, you are not caught:
it doesn't have time to notice you before you leave.)
If you were to do this in the configuration above,
marking your current position with parentheses,
your passage through the firewall would look like this:

Initial state:
 0   1   2   3   4   5   6
[S] [S] ... ... [S] ... [S]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 0:
 0   1   2   3   4   5   6
(S) [S] ... ... [S] ... [S]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
( ) [ ] ... ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 1:
 0   1   2   3   4   5   6
[ ] ( ) ... ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] (S) ... ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

Picosecond 2:
 0   1   2   3   4   5   6
[ ] [S] (.) ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] (.) ... [ ] ... [ ]
[S] [S]         [ ]     [ ]
[ ]             [ ]     [ ]
                [S]     [S]

Picosecond 3:
 0   1   2   3   4   5   6
[ ] [ ] ... (.) [ ] ... [ ]
[S] [S]         [ ]     [ ]
[ ]             [ ]     [ ]
                [S]     [S]

 0   1   2   3   4   5   6
[S] [S] ... (.) [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[ ]             [S]     [S]
                [ ]     [ ]

Picosecond 4:
 0   1   2   3   4   5   6
[S] [S] ... ... ( ) ... [ ]
[ ] [ ]         [ ]     [ ]
[ ]             [S]     [S]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] ... ... ( ) ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 5:
 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] (.) [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [S] ... ... [S] (.) [S]
[ ] [ ]         [ ]     [ ]
[S]             [ ]     [ ]
                [ ]     [ ]


Picosecond 6:
 0   1   2   3   4   5   6
[ ] [S] ... ... [S] ... (S)
[ ] [ ]         [ ]     [ ]
[S]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] ... ( )
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

In this situation, you are caught in layers 0 and 6,
because your packet entered the layer when its scanner
was at the top when you entered it.
You are not caught in layer 1, since the scanner moved
into the top of the layer once you were already there.

The severity of getting caught on a layer is equal to its depth multiplied by its range.
(Ignore layers in which you do not get caught.)
The severity of the whole trip is the sum of these values.
In the example above, the trip severity is 0*3 + 6*4 = 24.

Given the details of the firewall you've recorded,
if you leave immediately, what is the severity of your whole trip?

--- Part Two ---
Now, you need to pass through the firewall without being caught - easier said than done.

You can't control the speed of the packet, but you can delay it any number of picoseconds.
For each picosecond you delay the packet before beginning your trip, all security scanners
move one step. You're not in the firewall during this time; you don't enter layer 0 until
you stop delaying the packet.

In the example above, if you delay 10 picoseconds
(picoseconds 0 - 9), you won't get caught:

State after delaying:
 0   1   2   3   4   5   6
[ ] [S] ... ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

Picosecond 10:
 0   1   2   3   4   5   6
( ) [S] ... ... [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

 0   1   2   3   4   5   6
( ) [ ] ... ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 11:
 0   1   2   3   4   5   6
[ ] ( ) ... ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[S] (S) ... ... [S] ... [S]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 12:
 0   1   2   3   4   5   6
[S] [S] (.) ... [S] ... [S]
[ ] [ ]         [ ]     [ ]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] (.) ... [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Picosecond 13:
 0   1   2   3   4   5   6
[ ] [ ] ... (.) [ ] ... [ ]
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [S] ... (.) [ ] ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

Picosecond 14:
 0   1   2   3   4   5   6
[ ] [S] ... ... ( ) ... [ ]
[ ] [ ]         [ ]     [ ]
[S]             [S]     [S]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] ... ... ( ) ... [ ]
[S] [S]         [ ]     [ ]
[ ]             [ ]     [ ]
                [S]     [S]

Picosecond 15:
 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] (.) [ ]
[S] [S]         [ ]     [ ]
[ ]             [ ]     [ ]
                [S]     [S]

 0   1   2   3   4   5   6
[S] [S] ... ... [ ] (.) [ ]
[ ] [ ]         [ ]     [ ]
[ ]             [S]     [S]
                [ ]     [ ]

Picosecond 16:
 0   1   2   3   4   5   6
[S] [S] ... ... [ ] ... ( )
[ ] [ ]         [ ]     [ ]
[ ]             [S]     [S]
                [ ]     [ ]

 0   1   2   3   4   5   6
[ ] [ ] ... ... [ ] ... ( )
[S] [S]         [S]     [S]
[ ]             [ ]     [ ]
                [ ]     [ ]

Because all smaller delays would get you caught,
the fewest number of picoseconds you would need to delay to get through safely is 10.

What is the fewest number of picoseconds that you need to delay
the packet to pass through the firewall without being caught?
 */
object DataDefs:
  enum Turn:
    case Scanner, Hacker
    def next = this match
      case Scanner => Hacker
      case Hacker  => Scanner
  import Turn.*

  case class Layer(depth: Int, range: Int, scan: Int = 0, speed: Int = 1):
    def severity = depth * range
    def next =
      if scan == 0 then copy(scan = 1, speed = 1)
      else if scan == range - 1 then copy(scan = range - 2, speed = -1)
      else copy(scan = scan + speed)

  case class Packet(depth: Int, caught: List[Layer]):
    def next = copy(depth = depth + 1)
    def caughtAt(layer: Layer) = Packet(depth + 1, layer :: caught)
    def severity = caught.map(_.severity).sum

  case class Firewall(layers: Map[Int, Layer], maxDepth: Int, packet: Packet, turn: Turn):
    def done = packet.depth == maxDepth
    def next = turn match
      case Scanner =>
        val nextLayers = layers.map((depth, layer) => (depth, layer.next))
        Firewall(nextLayers, maxDepth, packet, turn.next)
      case Hacker =>
        layers.get(packet.depth + 1) match
          case None => Firewall(layers, maxDepth, packet.next, turn.next)
          case Some(layer) =>
            val newPackt = if layer.scan == 0 then packet.caughtAt(layer) else packet.next
            Firewall(layers, maxDepth, newPackt, turn.next)

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line match
    case s"$depth: $range" => depth.toInt -> Layer(depth.toInt, range.toInt)

  def parse(lines: Seq[String]) = lines.map(parseLine).toMap

object Solving:
  import DataDefs.*, Turn.*

  def simulate(layers: Map[Int, Layer]) =
    val packet = Packet(-1, Nil)
    var firewall = Firewall(layers, layers.keys.max, packet, Hacker)
    while !firewall.done do firewall = firewall.next
    firewall.packet

  def solve1(lines: Seq[String]) = simulate(Parsing.parse(lines)).severity

  def solve2(lines: Seq[String]) =
    var layers = Parsing.parse(lines)
    var picoseconds = 0
    while simulate(layers).caught.nonEmpty do
      layers = layers.map((depth, layer) => (depth, layer.next))
      picoseconds += 1
    picoseconds

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "13.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 24
// Testing.result2 // part 2: 10

object Main:
  private lazy val lines = os.read.lines(os.pwd / "13.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 2688
// Main.result2 // part 2: 3876272 took 25 mins!
