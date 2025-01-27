object DataDefs:
  private lazy val alphabet = "abcdefghijklmnopqrstuvwxyz"
  private lazy val duos =
    for
      letter1 <- alphabet
      letter2 <- alphabet
      if letter1 != letter2
    yield s"$letter1$letter2"
  private lazy val abbas = duos.map(duo => duo + duo.reverse)                // part 1
  private lazy val abas  = duos.map(duo => duo + duo.head)                   // part 2
  private lazy val babs  = abas.map(aba => s"${aba(1)}${aba.head}${aba(1)}") // part 2
  private lazy val pairs = abas.zip(babs)

  case class IP(hypers: List[String], nonHypers: List[String]):
    lazy val supportsTLS: Boolean =
      nonHypers.exists(nonHyper => abbas.exists(nonHyper.contains(_))) &&
        hypers.forall(hyper => abbas.forall(!hyper.contains(_)))

    lazy val supportsSSL: Boolean = pairs.exists: (aba, bab) =>
      nonHypers.exists(_.contains(aba)) && hypers.exists(_.contains(bab))

object Parsing:
  import DataDefs.*

  @annotation.tailrec
  def parseLine(hypers: List[String])(nonHypers: List[String])(line: String): IP =
    if line.isEmpty then IP(hypers, nonHypers)
    else
      line match
        case s"$nonHyper[$hyper]$rest" =>
          parseLine(hyper :: hypers)(nonHyper :: nonHypers)(rest)
        case _ => parseLine(hypers)(line :: nonHypers)("")

  def parse(lines: Seq[String]): Seq[IP] = lines.map(parseLine(Nil)(Nil))

object Solving:
  def solve1(lines: Seq[String]) = Parsing.parse(lines).count(_.supportsTLS)
  def solve2(lines: Seq[String]) = Parsing.parse(lines).count(_.supportsSSL)

object Test:
  lazy val file  = os.pwd / "2016" / "07" / "07.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 2 out of the first 4
// Test.res2 // part 2: 3 out of the second 4

object Main:
  lazy val file  = os.pwd / "2016" / "07" / "07.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 118
// Main.res2 // part 2: 260
