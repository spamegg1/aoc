package AdventOfCode2021

object Day16:
  val hexToBinary = Map('0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
                        '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
                        '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
                        'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")

  def parse(hex: String): Seq[Int] = hex.flatMap(hexToBinary).toSeq.map(_.asDigit)
  def binary(digits: Seq[Int]): Long = java.lang.Long.parseLong(digits.mkString, 2)

  sealed trait Packet
  case class Literal(version: Long, value: Long) extends Packet
  case class Operator(version: Long, typeId: Long, packets: List[Packet]) extends Packet

  def decode(input: Seq[Int]): (Int, Packet) =
    val version = binary(input.take(3))
    val typeId = binary(input.slice(3, 6))
    val lengthTypeId = input(6)
    if typeId == 4 then decodeLiteral(version, input)
    else if lengthTypeId == 1 then decodeOperatorBitLength(version, typeId, input)
    else decodeOperatorSubPackets(version, typeId, input)

  def decodeLiteral(version: Long, input: Seq[Int]): (Int, Packet) =
    val (_, chunks, read) = Iterator.iterate((1, Seq.empty[Int], 6))(decodeNextChunk(input)).dropWhile(_._1 == 1).next()
    (read, Literal(version, binary(chunks)))

  def decodeNextChunk(input: Seq[Int])(prefix: Int, chunks: Seq[Int], read: Int): (Int, Seq[Int], Int) =
    val nextPrefix = input(read)
    val nextChunk = chunks ++ input.slice(read + 1, read + 5)
    val nextRead = read + 5
    (nextPrefix, nextChunk, nextRead)

  def decodeOperatorBitLength(version: Long, typeId: Long, input: Seq[Int]): (Int, Packet) =
    val subPackets = binary(input.slice(7, 18)).toInt
    val (read, packets) = Iterator.iterate((18, List.empty[Packet]))(decodeNextPacket(input)).drop(subPackets).next()
    (read, Operator(version, typeId, packets.reverse))

  def decodeOperatorSubPackets(version: Long, typeId: Long, input: Seq[Int]): (Int, Packet) =
    val subLength = binary(input.slice(7, 22)) + 22
    val (read, packets) = Iterator.iterate((22, List.empty[Packet]))(decodeNextPacket(input)).dropWhile(_._1 < subLength).next()
    (read, Operator(version, typeId, packets.reverse))

  def decodeNextPacket(input: Seq[Int])(read: Int, packets: List[Packet]): (Int, List[Packet]) =
    val (nextRead, nextPacket) = decode(input.drop(read))
    (read + nextRead, nextPacket :: packets)

  def versionSum(packet: Packet): Long = packet match
    case Literal(version, _) => version
    case Operator(version, _, packets) => version + packets.map(versionSum).sum

  def expressionValue(packet: Packet): Long = packet match
    case Literal(_, value) => value
    case Operator(_, typeId, packets) => (typeId, packets.map(expressionValue)) match
      case (0, values) => values.sum
      case (1, values) => values.product
      case (2, values) => values.min
      case (3, values) => values.max
      case (5, List(first, second)) => if first > second then 1L else 0L
      case (6, List(first, second)) => if first < second then 1L else 0L
      case (7, List(first, second)) => if first == second then 1L else 0L
      case _ => throw MatchError("Unreachable")

  def part1(input: Seq[String]): Long = input.map(parse).map(decode).map(_._2).map(versionSum).sum

  def part2(input: Seq[String]): Long = input.map(parse).map(decode).map(_._2).map(expressionValue).sum

  def main(args: Array[String]): Unit =
    val data = io.Source.fromResource("AdventOfCode2021/Day16.txt").getLines().toSeq
    println(part1(data))
    println(part2(data))
