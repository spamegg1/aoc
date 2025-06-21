object Solving:
  import java.lang.Long.parseLong

  def solve1(lines: Seq[String]) =
    val size = lines.head.size
    val gamma = for
      index <- 0 until size
      bitsAtIndex = lines.map(_(index))
      zeros       = bitsAtIndex.count(_ == '0')
      ones        = bitsAtIndex.count(_ == '1')
    yield if zeros > ones then '0' else '1'
    val epsilon = gamma.map(char => if char == '0' then '1' else '0')
    parseLong(gamma.mkString, 2) * parseLong(epsilon.mkString, 2)

  def solve2(lines: Seq[String]) =
    val size   = lines.head.size
    var oxygen = lines
    var co2    = lines
    for index <- 0 until size do
      val (oxygenBits, co2Bits)   = (oxygen.map(_(index)), co2.map(_(index)))
      val (oxygenZeros, co2Zeros) = (oxygenBits.count(_ == '0'), co2Bits.count(_ == '0'))
      val (oxygenOnes, co2Ones)   = (oxygenBits.count(_ == '1'), co2Bits.count(_ == '1'))
      if oxygenZeros > oxygenOnes then
        oxygen = if oxygen.size == 1 then oxygen else oxygen.filter(_(index) == '0')
      else oxygen = if oxygen.size == 1 then oxygen else oxygen.filter(_(index) == '1')
      if co2Zeros > co2Ones then
        co2 = if co2.size == 1 then co2 else co2.filter(_(index) == '1')
      else co2 = if co2.size == 1 then co2 else co2.filter(_(index) == '0')
    parseLong(oxygen.head.mkString, 2) * parseLong(co2.head.mkString, 2)

object Test:
  lazy val file  = os.pwd / "2021" / "03" / "03.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 198
// Test.res2 // part 2: 230

object Main:
  lazy val file  = os.pwd / "2021" / "03" / "03.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 775304
// Main.res2 // part 2: 1370737
