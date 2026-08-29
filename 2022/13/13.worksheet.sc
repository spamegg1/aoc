object DataDefs:
  enum Packet:
    case Num(n: Int)
    case Lst(packets: List[Packet])

    infix def compare(that: Packet): Int = (this, that) match
      case (Num(m), Num(n)) => m compare n
      case (Num(_), Lst(_)) => Lst(List(this)).compare(that)
      case (Lst(_), Num(_)) => compare(Lst(List(that)))
      case (Lst(p), Lst(q)) =>
        (p, q) match
          case (Nil, Nil)       => 0
          case (head :: _, Nil) => 1
          case (Nil, head :: _) => -1
          case (pHead :: ps, qHead :: qs) =>
            val res = pHead compare qHead
            if res == 0 then Lst(ps) compare Lst(qs) else res

    override def toString: String = this match
      case Num(n)       => s"$n"
      case Lst(packets) => s"[${packets.mkString(",")}]"

  import Packet.*

  given Ordering[Packet] with
    def compare(x: Packet, y: Packet): Int = x compare y

  extension (s: String)
    def toNum          = Num(s.toInt)
    def balance        = s.count(_ == '[') - s.count(_ == ']')
    def closingBracket = (0 until s.size).indexWhere(i => s.take(i + 1).balance == 0) + 1

  case class Pair(left: Packet, right: Packet):
    lazy val isOrdered = (left compare right) == -1

object Parsing:
  import DataDefs.*, Packet.*

  private def parsePacket(line: String): Packet = line match
    case s"[$inner]" => Lst(parseList(inner))
    case s"$num"     => num.toNum

  private def parseList(line: String): List[Packet] = line match
    case s""       => Nil
    case s",$rest" => parseList(rest)
    case s"[$rest" =>
      val closing      = line.closingBracket
      val (list, rest) = (line.take(closing), line.drop(closing))
      parsePacket(list) :: parseList(rest)
    case s"$num,$rest" => parsePacket(num) :: parseList(rest)
    case s"$num"       => List(parsePacket(num))

  def parse1(lines: String): Seq[Pair] = lines
    .split("\n\n")
    .view
    .map(_.split("\n"))
    .map(pair => Pair(parsePacket(pair.head), parsePacket(pair.last)))
    .toSeq

  def parse2(lines: String): Seq[Packet] = (lines + "\n[[2]]\n[[6]]")
    .split("\n\n")
    .view
    .flatMap(_.split("\n"))
    .map(parsePacket)
    .toSeq
    .sorted

object Solving:
  import DataDefs.*, Packet.*

  def solve1(lines: String) = Parsing
    .parse1(lines)
    .view
    .zipWithIndex
    .filter((pair, _) => pair.isOrdered)
    .map((_, index) => index + 1)
    .sum

  private val decoders = Seq(Lst(List(Lst(List(Num(2))))), Lst(List(Lst(List(Num(6))))))

  def solve2(lines: String) = Parsing
    .parse2(lines)
    .view
    .zipWithIndex
    .filter((packet, _) => decoders contains packet)
    .map((_, index) => index + 1)
    .product

object Test:
  val file  = os.pwd / "2022" / "13" / "13.test.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 13
// Test.res2 // part 2: 140

object Main:
  val file  = os.pwd / "2022" / "13" / "13.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 6086
// Main.res2 // part 2: 27930
