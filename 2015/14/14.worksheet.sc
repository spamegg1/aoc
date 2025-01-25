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
    val deer   = Parsing.parse(lines)
    val scores = collection.mutable.Map.from[Deer, Int](deer.map(_ -> 0))
    for second <- 1 until seconds do
      val distances = deer.map(d => d -> d.distance(second))
      val maxDist   = distances.map(_._2).max
      val leaders   = distances.filter((_, dist) => dist == maxDist)
      for (leader, _) <- leaders do scores.update(leader, scores(leader) + 1)
    scores.maxBy(_._2)._2

object Test:
  lazy val file  = os.pwd / "2015" / "14" / "14.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(1000)
  lazy val res2  = Solving.solve2(lines)(1000)
// Test.res1 // part 1: 1120
// Test.res2 // part 2: 689

object Main:
  lazy val file  = os.pwd / "2015" / "14" / "14.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(2503)
  lazy val res2  = Solving.solve2(lines)(2503)
// Main.res1 // part 1: 2640
// Main.res2 // part 2: 1102
