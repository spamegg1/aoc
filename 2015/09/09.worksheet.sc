/*
--- Day 9: All in a Single Night ---
Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have provided
him the distances between every pair of locations. He can start and end at any
two (different) locations he wants, but he must visit each location exactly
once. What is the shortest distance he can travel to achieve this?

For example, given the following distances:
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141

The possible routes are therefore:
Dublin -> London -> Belfast = 982
London -> Dublin -> Belfast = 605
London -> Belfast -> Dublin = 659
Dublin -> Belfast -> London = 659
Belfast -> Dublin -> London = 605
Belfast -> London -> Dublin = 982

The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is
605 in this example.

What is the distance of the shortest route?

 */
import scalax.collection.edges.UnDiEdgeImplicits
import scalax.collection.edges.labeled.{WUnDiEdge, WUnDiEdgeFactory}
import scalax.collection.mutable.Graph

object DataDefs:
  type City = String
  type Distance = Double
  case class Route(start: City, end: City, distance: Distance)

object Parsing:
  import DataDefs.*

  private def parseRoute(line: String): Route = line match
    case s"$start to $end = $distance" => Route(start, end, distance.toDouble)

  def parseRoutes(lines: Seq[String]): Seq[Route] = lines.map(parseRoute)

object Solving:
  import DataDefs.*

  private def populate(routes: Seq[Route]) =
    val graph: Graph[City, WUnDiEdge[City]] = Graph.empty
    for route <- routes do graph += route.start ~ route.end % route.distance
    graph

  def solve1(lines: Seq[String]) =
    val routes = Parsing.parseRoutes(lines)
    val graph = populate(routes)
    graph.Walk

  def solve2(lines: Seq[String]) =
    val routes = Parsing.parseRoutes(lines)
    val graph = populate(routes)
    graph

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "09" / "09.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 605
// Testing.result2 // part 2:

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "09" / "09.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1:
// Main.result2 // part 2:

def shortestPathLength(graph: Array[Array[Int]]): Int =
  import collection.mutable, util.boundary, boundary.break
  val initial = graph.indices.map(i => (i, 1 << i)) // (0, 1) (1, 2) (2, 4) ... (n, 2^n)

  // (Node, Visited) => Length
  // Visited is represented as a bitmask. A Set would have been fine also.
  val lengths = mutable.Map.from(initial.map(_ -> 0)) // (0, 1) -> 0, (1, 2) -> 0, ...
  val q = mutable.Queue.from(initial) // (0, 1) (1, 2) (2, 4) ... (n, 2^n)
  val allVisited = (1 << graph.size) - 1 // 2^n - 1 = 111...1

  boundary:
    while q.nonEmpty do
      val (n, v) = q.dequeue() // // (0, 1) (1, 10) (2, 100) ... (n, 100...0)
      val l = lengths(n, v) + 1 // this +1 should instead be edge weight

      graph(n).foreach: n2 => // graph(n): 3 7 8
        // v       = 0010..0000
        // 1 << n2 = 000...1000
        // v2      = 0010..1000
        val v2 = v | (1 << n2)
        if v2 == allVisited then break(l)
        val l2 = lengths.getOrElse((n2, v2), Int.MaxValue)
        if l2 > l then
          lengths((n2, v2)) = l
          q.enqueue((n2, v2))
    0
