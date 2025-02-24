package aoc2019.day06

import scalax.collection.edges.{UnDiEdge, UnDiEdgeImplicits}
import scalax.collection.immutable.Graph

object Parsing:
  def parse(lines: Seq[String]) = Graph.from:
    lines.view
      .map(_.split(')'))
      .map(arr => arr(0) ~ arr(1))
      .toList

object Solving:
  def solve1(lines: Seq[String]) =
    val graph = Parsing.parse(lines)
    val root  = graph.get("COM")
    graph
      .outerNodeTraverser(root)
      .map(label => root.pathTo(graph.get(label)).get.length)
      .sum

  def solve2(lines: Seq[String]) =
    val graph = Parsing.parse(lines)
    val you   = graph.get("YOU")
    val santa = graph.get("SAN")
    you.shortestPathTo(santa).get.length - 2

object Test:
  lazy val file  = os.pwd / "2019" / "06" / "06.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2019" / "06" / "06.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 54
  println(Test.res2) // part 2: 4
  println(Main.res1) // part 1: 145250
  println(Main.res2) // part 2: 274
