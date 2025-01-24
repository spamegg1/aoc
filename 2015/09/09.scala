package aoc
package y2015
package day09

import collection.mutable.{Map => MMap, Queue => MQueue}

object DataDefs:
  type City  = String
  type Dist  = Int
  type Route = (start: City, end: City, dist: Dist)
  type Path  = (City, Set[City])
  type Graph = MMap[City, MMap[City, Dist]]

object Parsing:
  import DataDefs.*

  private def parseRoute(line: String): Route = line match
    case s"$start to $end = $distance" => (start, end, distance.toInt)

  def parseRoutes(lines: Seq[String]): Seq[Route] = lines.map(parseRoute)

object Solving:
  import DataDefs.*

  private def populate(routes: Seq[Route]): Graph =
    val graph = MMap.empty[City, MMap[City, Dist]]
    for (start, end, dist) <- routes do
      graph.get(start) match
        case None       => graph += start -> MMap(end -> dist)
        case Some(mmap) => mmap += end    -> dist
      graph.get(end) match
        case None       => graph += end  -> MMap(start -> dist)
        case Some(mmap) => mmap += start -> dist
    graph

  // part 1 is solved with a modified Dijkstra approach
  private def shortest(graph: Graph) =
    val cities = graph.keys.toSet
    val costs  = MMap.empty[Path, Dist]
    val queue  = MQueue.empty[Path]

    for city <- cities do
      queue.enqueue((city, Set(city)))
      costs((city, Set(city))) = 0

    while queue.nonEmpty do
      val (current, visited) = queue.dequeue()
      val neighbors          = graph(current)

      for (neighbor, dist) <- neighbors do
        val newPath = visited + neighbor
        val oldCost = costs.getOrElse((neighbor, newPath), Int.MaxValue)
        val newCost = costs(current, visited) + dist

        if oldCost > newCost then
          queue.enqueue((neighbor, newPath))
          costs((neighbor, newPath)) = newCost

    val shortest = cities.minBy(costs(_, cities))
    costs(shortest, cities)

  // part 2 is solved via brute force, calculate path lengths of all permutations
  // this is slow, it only works because data has 8 cities = 40320 permutations.
  private def longest(graph: Graph) =
    val cities = graph.keys.toSeq.permutations.toSeq
    val costs = cities.map: perm =>
      perm.init
        .zip(perm.tail)
        .map((start, end) => graph(start)(end))
        .sum
    costs.max

  private def solve(fun: Graph => Int)(lines: Seq[String]) =
    val routes = Parsing.parseRoutes(lines)
    val graph  = populate(routes)
    fun(graph)

  val solve1 = solve(shortest)
  val solve2 = solve(longest)

object Test:
  lazy val file  = os.pwd / "2015" / "09" / "09.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2015" / "09" / "09.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 605
  println(Test.res2) // part 2: 982
  println(Main.res1) // part 1: 141
  println(Main.res2) // part 2: 736
