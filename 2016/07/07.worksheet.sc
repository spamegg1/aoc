/*
--- Day 7: Internet Protocol Version 7 ---
While snooping around the local network of EBHQ,
you compile a list of IP addresses
(they're IPv7, of course; IPv6 is much too limited).
You'd like to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.
An ABBA is any four-character sequence which consists of a pair of
two different characters followed by the reverse of that pair, such as xyyx or abba.
However, the IP also must not have an ABBA within any hypernet sequences,
which are contained by square brackets.

For example:
  abba[mnop]qrst supports TLS (abba outside square brackets).
  abcd[bddb]xyyx does not support TLS (bddb is within square brackets,
    even though xyyx is outside square brackets).
  aaaa[qwer]tyui does not support TLS (aaaa is invalid;
    the interior characters must be different).
  ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets,
    even though it's within a larger string).

How many IPs in your puzzle input support TLS?

--- Part Two ---
You would also like to know which IPs support SSL (super-secret listening).
An IP supports SSL if it has an Area-Broadcast Accessor, or ABA,
anywhere in the supernet sequences (outside any square bracketed sections),
and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences.
An ABA is any three-character sequence which consists of the same character twice
with a different character between them, such as xyx or aba.
A corresponding BAB is the same characters but in reversed positions:
yxy and bab, respectively.

For example:
  aba[bab]xyz supports SSL
    (aba outside square brackets with corresponding bab within square brackets).
  xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
  aaa[kek]eke supports SSL
    (eke in supernet with corresponding kek in hypernet;
    the aaa sequence is not related, because the interior character must be different).
  zazbz[bzb]cdb supports SSL
    (zaz has no corresponding aza, but zbz has a corresponding bzb,
    even though zaz and zbz overlap).

How many IPs in your puzzle input support SSL?
 */
object DataDefs:
  private lazy val alphabet = "abcdefghijklmnopqrstuvwxyz"
  private lazy val duos =
    for
      letter1 <- alphabet
      letter2 <- alphabet
      if letter1 != letter2
    yield s"$letter1$letter2"
  private lazy val abbas = duos.map(duo => duo + duo.reverse) // part 1
  private lazy val abas = duos.map(duo => duo + duo.head) // part 2
  private lazy val babs = abas.map(aba => s"${aba(1)}${aba.head}${aba(1)}") // part 2
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

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "07.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 2 out of the first 4
Testing.result2 // part 2: 3 out of the second 4

object Main:
  private lazy val lines = os.read.lines(os.pwd / "07.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 118
Main.result2 // part 2: 260
