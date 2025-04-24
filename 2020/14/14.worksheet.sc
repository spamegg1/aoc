object DataDefs:
  type Memory = Map[Long, Long]
  type Next   = (Computer, Instr) => Computer
  enum Instr:
    case Mask(mask: String)
    case Load(addr: Long, value: Long)
  import Instr.*

  extension (s: String)
    def fromBinaryString: Long = java.lang.Long.parseLong(s, 2) // part 1
    def addresses              = Seq(s).allAddresses            // part 2

  extension (ss: Seq[String]) // part 2
    def nextAddresses = ss.flatMap: s =>
      if !s.contains("X") then Seq(s)
      else Seq(s.replaceFirst("X", "0"), s.replaceFirst("X", "1"))

    @annotation.tailrec
    def allAddresses: Seq[String] =
      val sss = ss.nextAddresses
      if ss == sss then ss else sss.allAddresses

  extension (value: Long) // part 1
    def modify(char: Char)(mask: String): String = value.toBinaryString.reverse
      .padTo(36, '0')
      .reverse
      .zip(mask)
      .map((b, m) => if m == char then b else m)
      .mkString
    def masked = value.modify('X') // part 1
    def floaty = value.modify('0') // part 2

  case class Computer(curMask: String, memory: Memory):
    def next1(instr: Instr) = instr match // part 1
      case Mask(mask) => copy(curMask = mask)
      case Load(addr, value) =>
        val newValue = value.masked(curMask).fromBinaryString
        copy(memory = memory.updated(addr, newValue))

    def next2(instr: Instr) = instr match // part 2
      case Mask(mask) => copy(curMask = mask)
      case Load(addr, value) =>
        val addresses = addr.floaty(curMask).addresses
        copy(memory = memory ++ addresses.map(a => a.fromBinaryString -> value).toMap)

    lazy val sumOfNonzero = memory.values.filter(_ != 0L).sum // parts 1, 2

object Parsing:
  import DataDefs.*, Instr.*

  private def parseLine(line: String) = line match
    case s"mask = $mask"        => Mask(mask)
    case s"mem[$addr] = $value" => Load(addr.toLong, value.toLong)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve(next: Next)(lines: Seq[String]) =
    val instrs   = Parsing.parse(lines)
    var computer = Computer("", Map())
    for instr <- instrs do computer = next(computer, instr)
    computer.sumOfNonzero

  val solve1 = solve((c, i) => c.next1(i))
  val solve2 = solve((c, i) => c.next2(i))

object Test:
  lazy val file1  = os.pwd / "2020" / "14" / "14.test.input.1.txt"
  lazy val file2  = os.pwd / "2020" / "14" / "14.test.input.2.txt"
  lazy val lines1 = os.read.lines(file1)
  lazy val lines2 = os.read.lines(file2)
  lazy val res1   = Solving.solve1(lines1)
  lazy val res2   = Solving.solve2(lines2)
// Test.res1 // part 1: 165
// Test.res2 // part 2: 208

object Main:
  lazy val file  = os.pwd / "2020" / "14" / "14.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 11884151942312
// Main.res2 // part 2: 2625449018811
