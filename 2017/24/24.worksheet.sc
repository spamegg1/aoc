object DataDefs:
  type Comp = (a: Int, b: Int, str: Int)
  extension (c: Comp)
    def matches(n: Int): Boolean = n == c.a || n == c.b
    def passthrough: Boolean     = c.a == c.b
    def opposite(n: Int): Int    = if n == c.a then c.b else c.a

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Comp =
    val Array(a, b) = line.split("/").map(_.toInt)
    (a = a, b = b, str = a + b)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def build(
      comps: Seq[Comp],
      current: Int,
      depth: Int,
      total: Int,
      ord: Ordering[(Int, Int)]
  ): (Int, Int) =
    val candidates = comps.filter(_.matches(current))
    val shortlist  = candidates.find(_.passthrough).map(Seq(_)).getOrElse(candidates)
    if shortlist.isEmpty then depth -> total
    else
      shortlist
        .map: next =>
          build(
            comps.diff(Seq(next)),
            next.opposite(current),
            depth + 1,
            total + next.str,
            ord
          )
        .max(using ord)

  def solve(lines: Seq[String])(ordering: Ordering[(Int, Int)]) =
    val comps = Parsing.parse(lines)
    build(comps, 0, 0, 0, ordering)._2

  def solve1(lines: Seq[String]) = solve(lines)(Ordering.by(_._2))
  def solve2(lines: Seq[String]) = solve(lines)(Ordering.by(identity))

object Test:
  lazy val file  = os.pwd / "2017" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 31
// Test.res2 // part 2: 19

object Main:
  lazy val file  = os.pwd / "2017" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1511
// Main.res2 // part 2: 1471
