/*
--- Day 4: Security Through Obscurity ---
Finally, you come across an information kiosk with a list of rooms. Of course, the list is
encrypted and full of decoy data, but the instructions to decode the list are barely
hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed
by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the
encrypted name, in order, with ties broken by alphabetization. For example:
    aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are
        a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
    a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied
      (1 of each), the first five are listed alphabetically.
    not-a-real-room-404[oarel] is a real room.
    totally-real-room-200[decoy] is not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.
What is the sum of the sector IDs of the real rooms?

--- Part Two ---
With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly
unbreakable without the right software. However, the information kiosk designers at Easter
Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times
equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on.
Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
What is the sector ID of the room where North Pole objects are stored?
 */
object DataDefs:
  type EncryptedName = String
  type SectorId = Int
  type Checksum = String

  val charToNumber = "abcdefghijklmnopqrstuvwxyz".zipWithIndex.toMap
  val numberToChar = charToNumber.map((char, int) => (int, char))

  case class Room(name: EncryptedName, sectorID: SectorId, checksum: Checksum):
    private lazy val correctChecksum = name
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .groupMapReduce(_._2)(_._1.toString)(_ + _)
      .toList
      .sortBy(-_._1)
      .map(_._2.sorted)
      .mkString
      .take(5)

    val decryptOne: Char => Char = char =>
      val index = charToNumber(char)
      val newIndex = (index + sectorID) % 26
      numberToChar(newIndex)

    lazy val isReal = checksum == correctChecksum // part 1
    lazy val decrypt = name.map(decryptOne) // part 2

object Parsing:
  import DataDefs.*

  extension (line: String)
    def toRoom = line match
      case s"$nameAndId[$checksum]" =>
        val stuff = nameAndId.split("-")
        val (name, id) = (stuff.init.mkString, stuff.last.toInt)
        Room(name, id, checksum)

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

object Testing:
  import Parsing.*
  private lazy val lines = os.read.lines(os.pwd / "2016" / "04" / "04.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = "qzmt-zixmtkozy-ivhz-343[zimth]".toRoom.decrypt
// Testing.result1 // part 1: 1514
// Testing.result2 // part 2: veryencryptedname

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2016" / "04" / "04.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 158835
// Main.result2 // part 2: Some(993)
