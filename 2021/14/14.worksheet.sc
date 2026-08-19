object DataDefs:
  type Quant = Map[Char, Long]
  type Pairs = Map[String, Long]
  type Rule  = (String, Char)
  type Rules = Map[String, Char]

  extension [A](map: Map[A, Long])
    def increase(key: A, amount: Long): Map[A, Long] =
      map.updated(key, map.getOrElse(key, 0L) + amount)

  extension [A](s: Seq[A]) def freqs = s.groupMapReduce(identity)(_ => 1L)(_ + _)

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$a -> $b" => (a, b.head)

  def parseRules(lines: Seq[String]): Rules = lines.map(parseRule).toMap
  def parsePairs(template: String): Pairs   = template.sliding(2).toSeq.freqs

  def parse(lines: Seq[String], template: String): (Quant, Pairs, Rules) =
    (template.freqs, parsePairs(template), parseRules(lines))

object Solving:
  import DataDefs.*

  def step(rules: Rules)(pairs: Pairs, quant: Quant): (Pairs, Quant) =
    pairs.foldLeft[(Pairs, Quant)]((Map(), quant)):
      case ((curPairs, qty), (key, value)) =>
        val nextPairs = curPairs
          .increase(key.updated(0, rules(key)), value)
          .increase(key.updated(1, rules(key)), value)
        val nextQuantity = qty.increase(rules(key), value)
        (nextPairs, nextQuantity)

  def insertion(quant: Quant, pairs: Pairs, rules: Rules)(steps: Int): Long =
    val (_, result) = Iterator
      .iterate((pairs, quant))(step(rules))
      .drop(steps)
      .next()
    result.values.max - result.values.min

  def solve(steps: Int)(lines: Seq[String], template: String): Long =
    val (quant, pairs, rules) = Parsing.parse(lines, template)
    insertion(quant, pairs, rules)(steps)

  val solve1 = solve(10)
  val solve2 = solve(40)

object Test:
  val file     = os.pwd / "2021" / "14" / "14.test.input.txt"
  val lines    = os.read.lines(file)
  val template = "NNCB"
  val res1     = Solving.solve1(lines, template)
  val res2     = Solving.solve2(lines, template)
// Test.res1 // part 1: 1588
// Test.res2 // part 2: 2188189693529

object Main:
  val file     = os.pwd / "2021" / "14" / "14.input.txt"
  val template = "PBVHVOCOCFFNBCNCCBHK"
  val lines    = os.read.lines(file)
  val res1     = Solving.solve1(lines, template)
  val res2     = Solving.solve2(lines, template)
// Main.res1 // part 1:
// Main.res2 // part 2:
