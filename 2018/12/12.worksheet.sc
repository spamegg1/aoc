import collection.mutable.ArrayDeque

object DataDefs:
  case class Rule(pattern: String, res: String)

  case class State(pots: String, rules: Seq[Rule], min: Int = 0, max: Int):
    lazy val nextPots = s"....$pots...."
      .sliding(5)
      .map: five =>
        rules.find(_.pattern == five) match
          case None        => "."
          case Some(value) => value.res
      .mkString
    lazy val newMin = min - (2 - nextPots.takeWhile(_ == '.').size)
    lazy val newMax = max + (2 - nextPots.reverse.takeWhile(_ == '.').size)
    def newPots     = nextPots.drop(2 + newMin - min).dropRight(2 - newMax + max)
    def next        = copy(pots = newPots, min = newMin, max = newMax)
    def plants = pots
      .zip(min to max)
      .filter(_._1 == '#')
      .map(_._2)
      .sum
    override def toString() = pots

  object State:
    def apply(initial: String, rules: Seq[Rule]) =
      new State(initial, rules, 0, initial.size - 1)

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$pattern => $res" => Rule(pattern, res)

  def parseRules(lines: Seq[String]) = lines map parseRule

  def parse(lines: Seq[String]) = State(lines.head, parseRules(lines.tail))

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(generations: Int) =
    var state = Parsing.parse(lines)
    for _ <- 1 to generations do state = state.next
    state.plants

  def solve2(lines: Seq[String])(generations: Long) =
    var state = Parsing.parse(lines)
    var count = 0
    val memo  = collection.mutable.Map(count -> state.plants)

    // for _ <- 1 to 200 do // find pattern in plant counts
    //   state = state.next
    //   count += 1
    //   memo(count) = state.plants
    // os.write(os.pwd / "2018" / "12" / "memo.txt", memo.mkString("\n"))

    // starting gen 98, every gen increases plants by 51
    // at gen 98, the plant count is 6193
    (generations - 98L) * 51L + 6193L

object Test:
  lazy val file  = os.pwd / "2018" / "12" / "12.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(20)
// Test.res1 // part 1: 325

object Main:
  lazy val file  = os.pwd / "2018" / "12" / "12.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(20)
  lazy val res2  = Solving.solve2(lines)(50000000000L)
// Main.res1 // part 1: 3421
// Main.res2 // part 2: 2550000001195
