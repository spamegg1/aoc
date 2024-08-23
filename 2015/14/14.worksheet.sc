/*
--- Day 14: Reindeer Olympics ---
This year is the Reindeer Olympics!
Reindeer can fly at high speeds, but must rest occasionally to recover their energy.
Santa would like to know which of his reindeer is fastest, and so he has them race.

Reindeer can only either be flying (always at their top speed)
or resting (not moving at all), and always spend whole seconds in either state.

For example, suppose you have the following Reindeer:
  Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
  Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

After one second, Comet has gone 14 km, while Dancer has gone 16 km.
After ten seconds, Comet has gone 140 km, while Dancer has gone 160 km.
On the eleventh second, Comet begins resting (staying at 140 km),
  and Dancer continues on for a total distance of 176 km.
On the 12th second, both reindeer are resting.
They continue to rest until the 138th second, when Comet flies for another ten seconds.
On the 174th second, Dancer flies for another 11 seconds.

In this example, after the 1000th second, both reindeer are resting,
and Comet is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
So, in this situation, Comet would win (if the race ended at 1000 seconds).

Given the descriptions of each reindeer (in your puzzle input),
after exactly 2503 seconds, what distance has the winning reindeer traveled?

--- Part Two ---
Seeing how reindeer move in bursts, Santa decides
he's not pleased with the old scoring system.

Instead, at the end of each second, he awards
one point to the reindeer currently in the lead.
(If there are multiple reindeer tied for the lead, they each get one point.)
He keeps the traditional 2503 second time limit, of course,
as doing otherwise would be entirely ridiculous.

Given the example reindeer from above, after the first second,
Dancer is in the lead and gets one point.
He stays in the lead until several seconds into Comet's second burst:
after the 140th second, Comet pulls into the lead and gets his first point.
Of course, since Dancer had been in the lead for the 139 seconds before that,
he has accumulated 139 points by the 140th second.

After the 1000th second, Dancer has accumulated 689 points,
while poor Comet, our old champion, only has 312.
So, with the new scoring system, Dancer would win
(if the race ended at 1000 seconds).

Again given the descriptions of each reindeer
(in your puzzle input), after exactly 2503 seconds,
how many points does the winning reindeer have?
 */
object DataDefs:
  case class Deer(name: String, speed: Long, run: Long, rest: Long):
    def distance(seconds: Int): Long =
      val q = seconds / (run + rest) // q(run+rest) <= seconds < (q+1)(run+rest)
      if seconds < (q + 1) * run + q * rest then
        val lastRun = seconds - q * (run + rest)
        (q * run + lastRun) * speed
      else (q + 1) * run * speed // (q+1)run + q*rest <= seconds < (q+1)(run+rest)

object Parsing:
  import DataDefs.*
  def parseDeer(line: String): Deer = line match
    case s"$n can fly $sp km/s for $run seconds, but then must rest for $rest seconds." =>
      Deer(n, sp.toLong, run.toLong, rest.toLong)

  def parse(lines: Seq[String]) = lines map parseDeer

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(seconds: Int): Long = Parsing
    .parse(lines)
    .map(_.distance(seconds))
    .max

  def solve2(lines: Seq[String])(seconds: Int) =
    val deer = Parsing.parse(lines)
    val scores = collection.mutable.Map.from[Deer, Int](deer.map(_ -> 0))
    for second <- 1 until seconds do
      val distances = deer.map(d => d -> d.distance(second))
      val maxDist = distances.map(_._2).max
      val leaders = distances.filter((_, dist) => dist == maxDist)
      for (leader, _) <- leaders do scores.update(leader, scores(leader) + 1)
    scores.maxBy(_._2)._2

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "14.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(1000)
  lazy val result2 = Solving.solve2(lines)(1000)
Testing.result1 // part 1: 1120
Testing.result2 // part 2: 689

object Main:
  private lazy val lines = os.read.lines(os.pwd / "14.input.txt")
  lazy val result1 = Solving.solve1(lines)(2503)
  lazy val result2 = Solving.solve2(lines)(2503)
Main.result1 // part 1: 2640
Main.result2 // part 2: 1102
