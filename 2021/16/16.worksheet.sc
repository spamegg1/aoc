object DataDefs:
  val HexToBin = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  enum Packet:
    case Literal(version: Long, value: Long)
    case Operator(version: Long, typeId: Long, packets: List[Packet])

    def versionSum: Long = this match
      case Literal(version, _)           => version
      case Operator(version, _, packets) => version + packets.map(_.versionSum).sum

    def expressionValue: Long = this match
      case Literal(_, value) => value
      case Operator(_, typeId, packets) =>
        (typeId, packets.map(_.expressionValue)) match
          case (0, values)              => values.sum
          case (1, values)              => values.product
          case (2, values)              => values.min
          case (3, values)              => values.max
          case (5, List(first, second)) => if first > second then 1L else 0L
          case (6, List(first, second)) => if first < second then 1L else 0L
          case (7, List(first, second)) => if first == second then 1L else 0L
          case _                        => throw MatchError("Unreachable")
    end expressionValue
  end Packet
end DataDefs

object Parsing:
  import DataDefs.*
  def parse(hex: String): Seq[Int]   = hex.flatMap(HexToBin).toSeq.map(_.asDigit)
  def binary(digits: Seq[Int]): Long = java.lang.Long.parseLong(digits.mkString, 2)

object Solving:
  import DataDefs.*
  def solve1(line: String) = 0L
  def solve2(line: String) = 0L

object Test:
  val file  = os.pwd / "2021" / "16" / "16.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = lines map Solving.solve1
  val res2  = lines map Solving.solve2
// Test.res1 // part 1: 16, 12, 23, 31
// Test.res2 // part 2:

object Main:
  val file = os.pwd / "2021" / "16" / "16.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(line)
  val res2 = Solving.solve2(line)
// Main.res1 // part 1:
// Main.res2 // part 2:
