object Solving:
  def modify(a: String) =
    val b = a.reverse.map(c => if c == '0' then '1' else '0')
    s"${a}0$b"

  @annotation.tailrec
  def modifyUntil(s: String, len: Int): String =
    if s.length < len then modifyUntil(modify(s), len) else s.take(len)

  def check(s: String) = s
    .grouped(2)
    .map(pair => if pair(0) == pair(1) then '1' else '0')
    .mkString

  @annotation.tailrec
  def checksum(s: String): String =
    val c = check(s)
    if c.length % 2 == 0 then checksum(c) else c

  def solve(length: Int, input: String) = checksum(modifyUntil(input, length))

object Test:
  lazy val modifyTest   = Solving.modify("111100001010")
  lazy val checkTest    = Solving.check("110010110100")
  lazy val checksumTest = Solving.checksum("110010110100")
  lazy val res          = Solving.solve(20, "10000")
// Test.modifyTest   // 1111000010100101011110000
// Test.checkTest    // 110101
// Test.checksumTest // 100
// Test.res          // part 1: 01100

object Main:
  lazy val res1 = Solving.solve(272, "10001110011110000")
  lazy val res2 = Solving.solve(35651584, "10001110011110000")
// Main.res1 // part 1: 10010101010011101
// Main.res2 // part 2: 01100111101101111
