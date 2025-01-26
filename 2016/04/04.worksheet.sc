object DataDefs:
  type Encrypted = String
  type SectorId  = Int
  type Checksum  = String

  val charToNumber = "abcdefghijklmnopqrstuvwxyz".zipWithIndex.toMap
  val numberToChar = charToNumber.map((char, int) => (int, char))

  case class Room(name: Encrypted, sectorID: SectorId, checksum: Checksum):
    private lazy val correctChecksum = name
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .groupMapReduce(_._2)(_._1.toString)(_ + _)
      .toList
      .sortBy(-_._1)
      .map(_._2.sorted)
      .mkString
      .take(5)

    val decryptOne: Char => Char = char =>
      val index    = charToNumber(char)
      val newIndex = (index + sectorID) % 26
      numberToChar(newIndex)

    lazy val isReal  = checksum == correctChecksum // part 1
    lazy val decrypt = name.map(decryptOne)        // part 2

  extension (line: String)
    def toRoom = line match
      case s"$nameAndId[$checksum]" =>
        val stuff      = nameAndId.split("-")
        val (name, id) = (stuff.init.mkString, stuff.last.toInt)
        Room(name, id, checksum)

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Seq[Room] = lines.map(_.toRoom)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .filter(_.isReal)
    .map(_.sectorID)
    .sum

  def solve2(lines: Seq[String]) = Parsing
    .parse(lines)
    .find(_.decrypt == "northpoleobjectstorage")
    .map(_.sectorID)

object Test:
  import DataDefs.*
  lazy val file  = os.pwd / "2016" / "04" / "04.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = "qzmt-zixmtkozy-ivhz-343[zimth]".toRoom.decrypt
// Test.res1 // part 1: 1514
// Test.res2 // part 2: veryencryptedname

object Main:
  lazy val file  = os.pwd / "2016" / "04" / "04.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 158835
// Main.res2 // part 2: Some(993)
