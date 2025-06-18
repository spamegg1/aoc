package aoc2020.day19

object DataDefs:
  type Rules = Map[Int, Rule]
  enum Rule:
    case Letter(letter: String)
    case Redirect(key: Int)
    case Sequence(first: Int, second: Int)
    case Triple(first: Int, second: Int, third: Int)
    case Single(first: Int, second: Int)
    case Double(first: Int, second: Int, third: Int, fourth: Int)
    case Zero(size: Int)

    def prefix(line: String)(using rules: Rules): Option[Int] = this match
      case Letter(letter) => Option.when(line.take(1) == letter)(1)
      case Redirect(key)  => rules(key).prefix(line)
      case Sequence(first, second) =>
        rules(first)
          .prefix(line)
          .flatMap: length =>
            rules(second)
              .prefix(line.drop(length))
              .map(_ + length)
      case Triple(first, second, third) =>
        Sequence(first, second)
          .prefix(line)
          .flatMap: length =>
            rules(third)
              .prefix(line.drop(length))
              .map(_ + length)
      case Single(first, second) =>
        rules(first)
          .prefix(line)
          .orElse(rules(second).prefix(line))
      case Double(first, second, third, fourth) =>
        Sequence(first, second)
          .prefix(line)
          .orElse(Sequence(third, fourth).prefix(line))
      case Zero(size) =>
        var index  = line.length - size
        var length = 3 * size
        var found  = false
        while length <= line.length && !found do
          val left = line
            .take(index)
            .grouped(size)
            .forall(group => rules(42).prefix(group) == Some(size))
          val right = line
            .drop(index)
            .grouped(size)
            .forall(group => rules(31).prefix(group) == Some(size))
          index -= size
          length += 2 * size
          found = left && right
        Option.when(found)(line.length)
    end prefix

object Parsing:
  import DataDefs.*, Rule.*

  val letter   = "\"(\\w)\"".r
  val redirect = "(\\d+)".r
  val sequence = "(\\d+) (\\d+)".r
  val triple   = "(\\d+) (\\d+) (\\d+)".r
  val single   = "(\\d+) \\| (\\d+)".r
  val double   = "(\\d+) (\\d+) \\| (\\d+) (\\d+)".r

  def parseRules(lines: Seq[String]): Rules =
    val rules = lines.map: line =>
      val Array(id, pattern) = line.split(": ")
      val rule = pattern match
        case letter(letter)          => Letter(letter)
        case redirect(first)         => Redirect(first.toInt)
        case sequence(first, second) => Sequence(first.toInt, second.toInt)
        case triple(first, second, third) =>
          Triple(first.toInt, second.toInt, third.toInt)
        case single(first, second) => Single(first.toInt, second.toInt)
        case double(first, second, third, fourth) =>
          Double(first.toInt, second.toInt, third.toInt, fourth.toInt)
      id.toInt -> rule
    rules.toMap

  def parse(lines: Seq[String]): (Rules, Seq[String]) =
    val index    = lines.indexOf("")
    val rules    = parseRules(lines.take(index))
    val messages = lines.drop(index + 1)
    (rules, messages)

object Solving:
  import DataDefs.*, Rule.*

  def solve1(lines: Seq[String]) =
    val (rules, messages) = Parsing.parse(lines)
    given Rules           = rules
    messages.count: message =>
      rules(0)
        .prefix(message)
        .exists(_ == message.length)

  def solve2(lines: Seq[String]) =
    val size                 = 8
    val (original, messages) = Parsing.parse(lines)
    val rules                = original.updated(0, Zero(size))
    given Rules              = rules
    messages.count: message =>
      rules(0)
        .prefix(message)
        .exists(_ == message.length)

object Test:
  lazy val file1  = os.pwd / "2020" / "19" / "19.test.input.1.txt"
  lazy val file2  = os.pwd / "2020" / "19" / "19.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)

object Main:
  lazy val file  = os.pwd / "2020" / "19" / "19.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 2
  println(Test.res2) // part 2: 0
  println(Main.res1) // part 1: 180
  println(Main.res2) // part 2: 323
