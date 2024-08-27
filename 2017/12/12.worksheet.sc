/*
--- Day 12: Digital Plumber ---
Walking along the memory banks of the stream,
you find a small village that is experiencing a little confusion:
  some programs can't communicate with each other.

Programs in this village communicate using a fixed system of pipes.
Messages are passed between programs using these pipes,
but most programs aren't connected to each other directly.
Instead, programs pass messages between each other until the
message reaches the intended recipient.

For some reason, though, some of these messages aren't ever
reaching their intended recipient, and the programs suspect
that some pipes are missing. They would like you to investigate.

You walk through the village and record the ID of each program
and the IDs with which it can communicate directly (your puzzle input).
Each program has one or more programs with which it can communicate,
and these pipes are bidirectional; if 8 says it can communicate with 11,
then 11 will say it can communicate with 8.

You need to figure out how many programs are in the group that contains program ID 0.

For example, suppose you go door-to-door like a
travelling salesman and record the following list:

0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5

In this example, the following programs are in the group that contains program ID 0:
  Program 0 by definition.
  Program 2, directly connected to program 0.
  Program 3 via program 2.
  Program 4 via program 2.
  Program 5 via programs 6, then 4, then 2.
  Program 6 via programs 4, then 2.

Therefore, a total of 6 programs are in this group;
all but program 1, which has a pipe that connects it to itself.

How many programs are in the group that contains program ID 0?

--- Part Two ---
There are more programs than just the ones in the group containing program ID 0.
The rest of them have no way of reaching that group,
and still might have no way of reaching each other.

A group is a collection of programs that can all communicate via pipes
either directly or indirectly. The programs you identified just a
moment ago are all part of the same group. Now, they would like you
to determine the total number of groups.

In the example above, there were 2 groups: one consisting of programs
0,2,3,4,5,6, and the other consisting solely of program 1.

How many groups are there in total?
 */
object DataDefs:
  // this is trivial if we use Scala Graph library. So let's do it ourselves.
  type Node = Int
  type Graph = Map[Node, Set[Node]]

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): (Node, Set[Node]) = line match
    case s"$node <-> $connected" => node.toInt -> connected.split(", ").map(_.toInt).toSet

  def parse(lines: Seq[String]) = lines.map(parseLine).toMap

object Solving:
  import DataDefs.*, collection.immutable.Queue

  @annotation.tailrec
  def component(connected: Set[Node])(queue: Queue[Int])(using graph: Graph): Set[Node] =
    if queue.isEmpty then connected
    else
      val (node, next) = queue.dequeue
      val nodes = graph(node)
      val newConnected = nodes ++ connected
      val newToExplore = nodes.filterNot(connected.contains)
      val newQueue = next.enqueueAll(newToExplore)
      component(newConnected)(newQueue)

  @annotation.tailrec
  private def findAndRemoveComponent(graph: Graph)(count: Int): Int =
    if graph.isEmpty then count
    else
      val node = graph.keys.head
      val nodeComponent = component(Set(node))(Queue(node))(using graph)
      val newGraph = graph.removedAll(nodeComponent)
      findAndRemoveComponent(newGraph)(count + 1)

  def solve1(lines: Seq[String]) =
    given Graph = Parsing.parse(lines)
    component(Set(0))(Queue(0)).size

  def solve2(lines: Seq[String]) = findAndRemoveComponent(Parsing.parse(lines))(0)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2017" / "12" / "12.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 6
// Testing.result2 // part 2: 2

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2017" / "12" / "12.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 380
// Main.result2 // part 2: 181
