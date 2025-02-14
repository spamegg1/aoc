object DataDefs:
  // this is trivial if we use Scala Graph library. So let's do it ourselves.
  type Node  = Int
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
      val nodes        = graph(node)
      val newConnected = nodes ++ connected
      val newToExplore = nodes.filterNot(connected.contains)
      val newQueue     = next.enqueueAll(newToExplore)
      component(newConnected)(newQueue)

  @annotation.tailrec
  private def findAndRemoveComponent(graph: Graph)(count: Int): Int =
    if graph.isEmpty then count
    else
      val node          = graph.keys.head
      val nodeComponent = component(Set(node))(Queue(node))(using graph)
      val newGraph      = graph.removedAll(nodeComponent)
      findAndRemoveComponent(newGraph)(count + 1)

  def solve1(lines: Seq[String]) =
    given Graph = Parsing.parse(lines)
    component(Set(0))(Queue(0)).size

  def solve2(lines: Seq[String]) = findAndRemoveComponent(Parsing.parse(lines))(0)

object Test:
  lazy val file  = os.pwd / "2017" / "12" / "12.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 6
// Test.res2 // part 2: 2

object Main:
  lazy val file  = os.pwd / "2017" / "12" / "12.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 380
// Main.res2 // part 2: 181
