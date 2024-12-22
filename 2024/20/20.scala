/*
--- Day 20: Race Condition ---
The Historians are quite pixelated again. This time, a massive,
black building looms over you - you're right outside the CPU!

While The Historians get to work, a nearby program sees that you're
idle and challenges you to a race. Apparently, you've arrived just in
time for the frequently-held race condition festival!

The race takes place on a particularly long and twisting code path;
programs compete to see who can finish in the fewest picoseconds.
The winner even gets their very own mutex!

They hand you a map of the racetrack (your puzzle input). For example:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

The map consists of track (.) - including the start (S) and end (E)
positions (both of which also count as track) - and walls (#).

When a program runs through the racetrack, it starts at the start position.
Then, it is allowed to move up, down, left, or right; each such move takes 1 picosecond. The goal is to reach the end position as quickly as possible. In this example racetrack, the fastest time is 84 picoseconds.

Because there is only a single path from the start to the end and the programs
all go the same speed, the races used to be pretty boring.
To make things more interesting, they introduced a new rule to the races:
  programs are allowed to cheat.

The rules for cheating are very strict. Exactly once during a race,
a program may disable collision for up to 2 picoseconds.
This allows the program to pass through walls as if they were regular track.
At the end of the cheat, the program must be back on normal track again;
otherwise, it will receive a segmentation fault and get disqualified.

So, a program could complete the course in 72 picoseconds (saving 12 picoseconds)
by cheating for the two moves marked 1 and 2:

###############
#...#...12....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Or, a program could complete the course in 64 picoseconds (saving 20 picoseconds)
by cheating for the two moves marked 1 and 2:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...12..#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 38 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.####1##.###
#...###.2.#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 64 picoseconds and takes the program directly to the end:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..21...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Each cheat has a distinct start position (the position where the cheat is activated,
just before the first move that is allowed to go through walls) and end position;
cheats are uniquely identified by their start position and end position.

In this example, the total number of cheats (grouped by the amount of time they save)
are as follows:
  There are 14 cheats that save 2 picoseconds.
  There are 14 cheats that save 4 picoseconds.
  There are 2 cheats that save 6 picoseconds.
  There are 4 cheats that save 8 picoseconds.
  There are 2 cheats that save 10 picoseconds.
  There are 3 cheats that save 12 picoseconds.
  There is one cheat that saves 20 picoseconds.
  There is one cheat that saves 36 picoseconds.
  There is one cheat that saves 38 picoseconds.
  There is one cheat that saves 40 picoseconds.
  There is one cheat that saves 64 picoseconds.

You aren't sure what the conditions of the racetrack will be like,
so to give yourself as many options as possible,
you'll need a list of the best cheats.
How many cheats would save you at least 100 picoseconds?

--- Part Two ---
The programs seem perplexed by your list of cheats.
Apparently, the two-picosecond cheating rule was deprecated several milliseconds ago!
The latest version of the cheating rule permits a single cheat
that instead lasts at most 20 picoseconds.

Now, in addition to all the cheats that were possible in just two picoseconds,
many more cheats are possible.
This six-picosecond cheat saves 76 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#1#####.#.#.###
#2#####.#.#...#
#3#####.#.###.#
#456.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Because this cheat has the same start and end positions as the one above,
it's the same cheat, even though the path taken during the cheat is different:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S12..#.#.#...#
###3###.#.#.###
###4###.#.#...#
###5###.#.###.#
###6.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Cheats don't need to use all 20 picoseconds;
cheats can last any amount of time up to and including 20 picoseconds
(but can still only end when the program is on normal track).
Any cheat time not used is lost; it can't be saved for another cheat later.

You'll still need a list of the best cheats,
but now there are even more to choose between.
Here are the quantities of cheats in this example that save 50 picoseconds or more:
  There are 32 cheats that save 50 picoseconds.
  There are 31 cheats that save 52 picoseconds.
  There are 29 cheats that save 54 picoseconds.
  There are 39 cheats that save 56 picoseconds.
  There are 25 cheats that save 58 picoseconds.
  There are 23 cheats that save 60 picoseconds.
  There are 20 cheats that save 62 picoseconds.
  There are 19 cheats that save 64 picoseconds.
  There are 12 cheats that save 66 picoseconds.
  There are 14 cheats that save 68 picoseconds.
  There are 12 cheats that save 70 picoseconds.
  There are 22 cheats that save 72 picoseconds.
  There are 4 cheats that save 74 picoseconds.
  There are 3 cheats that save 76 picoseconds.

Find the best cheats using the updated cheating rules.
How many cheats would save you at least 100 picoseconds?
 */
package aoc2024.day20

import scalax.collection.edges.UnDiEdge
import scalax.collection.edges.UnDiEdgeImplicits // for the ~ edge infix operator
import scalax.collection.mutable.{Graph => MGraph}

object DataDefs:
  val WALL = '#'

  enum Dir:
    case N, S, E, W
  import Dir.*

  type Pos   = (row: Int, col: Int)
  type Track = Seq[String]
  type Race  = MGraph[Pos, UnDiEdge[Pos]]

  extension (p: Pos)
    def move(dir: Dir): Pos = dir match
      case N => (p.row - 1, p.col)
      case S => (p.row + 1, p.col)
      case E => (p.row, p.col + 1)
      case W => (p.row, p.col - 1)
    def neighbors = Dir.values.map(p.move).toSeq
    def possible(using track: Track) = p.neighbors
      .filterNot(p => track(p.row)(p.col) == WALL)

  extension (scores: Seq[Int]) def freqs = scores.groupMapReduce(identity)(_ => 1)(_ + _)

object Parsing:
  import DataDefs.*

  def makeRace(using track: Track, size: Int): Race =
    val graph = MGraph.empty[Pos, UnDiEdge[Pos]]
    for
      row <- 1 until size - 1 // ignore surrounding walls
      col <- 1 until size - 1
      pos = (row, col)
      if track(row)(col) != WALL
    do pos.possible.foreach(p => graph += pos ~ p)
    graph

  def mutateRace(graph: Race, newPos: Pos, oldPos: Pos)(using Track, Int): Race =
    graph -= oldPos
    newPos.possible.foreach(p => graph += newPos ~ p)
    graph

object Solving:
  import DataDefs.*

  def getScore(race: Race)(start: Pos, finish: Pos): Int =
    race.get(start).shortestPathTo(race.get(finish)).get.edges.size

  def solve1(start: Pos, finish: Pos)(saved: Int)(using track: Seq[String], size: Int) =
    val race        = Parsing.makeRace
    val normalScore = getScore(race)(start, finish)
    // println(s"normal: $normalScore") // 84 in test
    var oldPos = (0, 0)

    val savedTimes = for
      row <- 1 until size - 1
      col <- 1 until size - 1
      newPos = (row, col)
      if track(row)(col) == WALL
    yield
      val newRace = Parsing.mutateRace(race, newPos, oldPos)
      oldPos = newPos
      val cheatScore = getScore(newRace)(start, finish)
      val diff       = normalScore - cheatScore
      println(s"newPos: $newPos diff: $diff")
      diff

    savedTimes.freqs
      .filter(saved <= _._1)
      .map(_._2)
      .sum

  def solve2(using track: Seq[String]) = 0L

object Testing: // s=3,1 e=7,5 size=15 saved=100
  import DataDefs.*
  given Track      = os.read.lines(os.pwd / "2024" / "20" / "20.test.input.txt")
  given size: Int  = 15
  val start        = (3, 1)
  val finish       = (7, 5)
  lazy val result1 = Solving.solve1(start, finish)(10) // 2+3+1+1+1+1+1=10
  lazy val result2 = Solving.solve2
// Testing.result2 // part 2:

object Main: // s=67,75 e=43,71 size=141
  import DataDefs.*
  given Track      = os.read.lines(os.pwd / "2024" / "20" / "20.input.txt")
  given size: Int  = 141
  val start        = (67, 75)
  val finish       = (43, 71)
  lazy val result1 = Solving.solve1(start, finish)(100)
  lazy val result2 = Solving.solve2

@main
def run: Unit =
  // println(Testing.result1) // part 1: 10
  // 0->40,2->14,4->14,6->2,8->4,10->2,12->3,20->1,36->1,38->1,40->1,64->1
  // println(Main.result1) // part 1: 1378
  ()
