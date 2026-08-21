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

  type Data  = Seq[Int]
  type Packs = List[Packet]

  enum Packet:
    case Lit(ver: Long, value: Long)
    case Op(ver: Long, id: Long, packs: Packs)

    def verSum: Long = this match
      case Lit(ver, _)       => ver
      case Op(ver, _, packs) => ver + packs.map(_.verSum).sum

    def exprVal: Long = this match
      case Lit(_, value) => value
      case Op(_, id, packs) =>
        (id, packs.map(_.exprVal)) match
          case (0, values)     => values.sum
          case (1, values)     => values.product
          case (2, values)     => values.min
          case (3, values)     => values.max
          case (5, List(a, b)) => if a > b then 1L else 0L
          case (6, List(a, b)) => if a < b then 1L else 0L
          case (7, List(a, b)) => if a == b then 1L else 0L
          case _               => throw MatchError("Unreachable")
    end exprVal
  end Packet
end DataDefs

object Parsing:
  import DataDefs.*
  def parse(hex: String): Data   = hex.flatMap(HexToBin).toSeq.map(_.asDigit)
  def binary(digits: Data): Long = java.lang.Long.parseLong(digits.mkString, 2)

object Decoder:
  import DataDefs.*, Packet.*

  def decode(input: Data): (Int, Packet) =
    val ver      = Parsing.binary(input.take(3))
    val id       = Parsing.binary(input.slice(3, 6))
    val lengthId = input(6)
    if id == 4 then literal(ver, input)
    else if lengthId == 1 then opBitLength(ver, id, input)
    else opSubPacks(ver, id, input)

  def literal(ver: Long, input: Data): (Int, Packet) =
    val (_, chunks, read) = Iterator
      .iterate((1, Seq.empty[Int], 6))(nextChunk(input))
      .dropWhile(_._1 == 1)
      .next()
    (read, Lit(ver, Parsing.binary(chunks)))

  def nextChunk(input: Data)(prefix: Int, chunks: Data, read: Int): (Int, Data, Int) =
    val nextPrefix = input(read)
    val nextChunk  = chunks ++ input.slice(read + 1, read + 5)
    val nextRead   = read + 5
    (nextPrefix, nextChunk, nextRead)

  def opBitLength(ver: Long, id: Long, input: Data): (Int, Packet) =
    val subPackets = Parsing.binary(input.slice(7, 18)).toInt
    val (read, packets) = Iterator
      .iterate((18, List.empty[Packet]))(nextPacket(input))
      .drop(subPackets)
      .next()
    (read, Op(ver, id, packets.reverse))

  def opSubPacks(ver: Long, id: Long, input: Data): (Int, Packet) =
    val subLength = Parsing.binary(input.slice(7, 22)) + 22
    val (read, packets) = Iterator
      .iterate((22, List.empty[Packet]))(nextPacket(input))
      .dropWhile(_._1 < subLength)
      .next()
    (read, Op(ver, id, packets.reverse))

  def nextPacket(input: Data)(read: Int, packs: Packs): (Int, Packs) =
    val (nextRead, nextPacket) = decode(input.drop(read))
    (read + nextRead, nextPacket :: packs)
end Decoder

object Solving:
  import DataDefs.*
  def solve1(line: String) = Decoder.decode(Parsing.parse(line))._2.verSum
  def solve2(line: String) = Decoder.decode(Parsing.parse(line))._2.exprVal

object Test:
  val file  = os.pwd / "2021" / "16" / "16.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = lines map Solving.solve1
  val res2  = lines map Solving.solve2
// Test.res1 // part 1: 16, 12, 23, 31, 14, 8, 15, 11, 13, 19, 16, 20
// Test.res2 // part 2: 16, 12, 23, 31, 14, 8, 15, 11, 13, 19, 16, 20

object Main:
  val file = os.pwd / "2021" / "16" / "16.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(line)
  val res2 = Solving.solve2(line)
// Main.res1 // part 1: 925
// Main.res2 // part 2: 342997120375
