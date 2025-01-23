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

The shortest of these is London -> Dublin -> Belfast = 605,
and so the answer is 605 in this example.

What is the distance of the shortest route?

--- Part Two ---
The next year, just to show off,
Santa decides to take the route with the longest distance instead.

He can still start and end at any two (different) locations he wants,
and he still must visit each location exactly once.

For example, given the distances above, the longest route would be 982
via (for example) Dublin -> London -> Belfast.

What is the distance of the longest route?
 */
package aoc2015.day09

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
  lazy val file    = os.pwd / "2015" / "09" / "09.test.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)

object Main:
  lazy val file    = os.pwd / "2015" / "09" / "09.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.result1) // part 1: 605
  println(Test.result2) // part 2: 982
  println(Main.result1) // part 1: 141
  println(Main.result2) // part 2: 736
