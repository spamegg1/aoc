/*
--- Day 23: LAN Party ---
As The Historians wander around a secure area at Easter Bunny HQ,
you come across posters for a LAN party scheduled for today!
Maybe you can find it; you connect to a nearby datalink port
and download a map of the local network (your puzzle input).

The network map provides a list of every connection between two computers. For example:

kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn

Each line of text in the network map represents a single connection;
the line kh-tc represents a connection between the computer named kh
and the computer named tc. Connections aren't directional;
tc-kh would mean exactly the same thing.

LAN parties typically involve multiplayer games,
so maybe you can locate it by finding groups of connected computers.
Start by looking for sets of three computers where each computer in the
set is connected to the other two computers.

In this example, there are 12 such sets of three inter-connected computers:

aq,cg,yn
aq,vc,wq
co,de,ka
co,de,ta
co,ka,ta
de,ka,ta
kh,qp,ub
qp,td,wh
tb,vc,wq
tc,td,wh
td,wh,yn
ub,vc,wq

If the Chief Historian is here, and he's at the LAN party,
it would be best to know that right away.
You're pretty sure his computer's name starts with t,
so consider only sets of three computers where at least one
computer's name starts with t. That narrows the list down to
7 sets of three inter-connected computers:

co,de,ta
co,ka,ta
de,ka,ta
qp,td,wh
tb,vc,wq
tc,td,wh
td,wh,yn

Find all the sets of three inter-connected computers.
How many contain at least one computer with a name that starts with t?

--- Part Two ---
There are still way too many results to go through them all.
You'll have to find the LAN party another way and go there yourself.

Since it doesn't seem like any employees are around,
you figure they must all be at the LAN party.
If that's true, the LAN party will be the largest set of computers
that are all connected to each other.
That is, for each computer at the LAN party,
that computer will have a connection to every other computer at the LAN party.

In the above example, the largest set of computers that are all connected
to each other is made up of co, de, ka, and ta.
Each computer in this set has a connection to every other computer in the set:

ka-co
ta-co
de-co
ta-ka
de-ta
ka-de

The LAN party posters say that the password to get into the LAN party
is the name of every computer at the LAN party,
sorted alphabetically, then joined together with commas.
(The people running the LAN party are clearly a bunch of nerds.)
In this example, the password would be co,de,ka,ta.

What is the password to get into the LAN party?

 */
package aoc2024.day23

import scalax.collection.mutable.Graph
import scalax.collection.edges.{UnDiEdge, UnDiEdgeImplicits}

object DataDefs:
  type Comp    = String
  type Comps   = Seq[Comp]
  type Connect = (Comp, Comp)
  type LAN     = Map[Comp, Comps]

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Seq[Connect] = line match
    case s"$c1-$c2" => Seq(c1 -> c2, c2 -> c1)

  def parse(lines: Seq[String]): LAN = lines.flatMap(parseLine).groupMap(_._1)(_._2)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) =
    val lan = Parsing.parse(lines)
    val ts  = lan.filterKeys(_.startsWith("t")).toMap
    val cliques = for
      (t, neighbors) <- ts
      Seq(n1, n2)    <- neighbors.combinations(2)
      if lan(n1).contains(t) && lan(n1).contains(n2) &&
        lan(n2).contains(t) && lan(n2).contains(n1) // clique of size 3
    yield Set(t, n1, n2)                            // order should not matter
    cliques.toSet.size                              // remove repetitions

  def findClique(comp: Comp)(using lan: LAN) =
    // comp has neighbors: lan(comp) = n1, n2, n3, ...
    // n1 has to be connected to every n2, n3, ... so check this
    // then n2 has to be connected to every n3, n4, ...
    // def helper(c: Comp)(cs: Comps): Boolean =
    ???

  def solve2(lines: Seq[String]) =
    given lan: LAN = Parsing.parse(lines)
    val cliques    = lan.keys.flatMap(findClique)
    0L

object Testing:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "23" / "23.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)

object Main:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "23" / "23.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)

@main
def run: Unit =
  // println(Testing.result1) // part 1: 7
  // println(Testing.result2) // part 2: co,de,ka,ta
  println(Main.result1) // part 1: 1000
  // println(Main.result2) // part 2:
  ()
