object DataDefs:
  case class Policy(low: Int, hi: Int, char: Char)
  case class Password(policy: Policy, password: String):
    lazy val isValid1 =
      val count = password.count(_ == policy.char)
      policy.low <= count && count <= policy.hi
    lazy val isValid2 =
      Seq(password(policy.low - 1), password(policy.hi - 1))
        .count(_ == policy.char) == 1

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Password = line match
    case s"$low-$hi $char: $password" =>
      Password(Policy(low.toInt, hi.toInt, char(0)), password)

  def parse(lines: Seq[String]): Seq[Password] = lines.map(parseLine)

object Solving:
  def solve1(lines: Seq[String]): Long = Parsing
    .parse(lines)
    .count(_.isValid1)

  def solve2(lines: Seq[String]): Long = Parsing
    .parse(lines)
    .count(_.isValid2)

object Test:
  lazy val file  = os.pwd / "2020" / "02" / "02.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 2
// Test.res2 // part 2: 1

object Main:
  lazy val file  = os.pwd / "2020" / "02" / "02.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 416
// Main.res2 // part 2: 688
