import org.apache.commons.codec.digest.DigestUtils.md5Hex

object Solving:
  def findOneDigit(start: Long, input: String) =
    var number = start
    var hash   = ""
    while !hash.startsWith("00000") do
      hash = md5Hex(s"$input$number")
      number += 1
    (number, hash)

  def solve1(input: String) =
    var digitCount = 0
    var start      = 0L
    var password   = List[Char]()

    while digitCount < 8 do
      val (digit, hash) = findOneDigit(start, input)
      password = hash(5) :: password
      digitCount += 1
      start = digit + 1

    password.reverse.mkString

  def solve2(input: String) =
    var digitCount = 0
    var start      = 0L
    var password   = collection.mutable.Map[Int, Char]()

    while digitCount < 8 do
      val (digit, hash) = findOneDigit(start, input)
      val position      = hash(5).asDigit
      if position < 8 && !password.isDefinedAt(position) then
        password(position) = hash(6)
        digitCount += 1
      start = digit + 1

    password.toList
      .sortBy(_._1)
      .map(_._2)
      .mkString

object Test:
  lazy val input = "abc"
  lazy val res1  = Solving.solve1(input)
  lazy val res2  = Solving.solve2(input)
// Test.res1 // part 1: 18f47a30
// Test.res2 // part 2: 05ace8e3

object Main:
  lazy val input = "ojvtpuvg"
  lazy val res1  = Solving.solve1(input)
  lazy val res2  = Solving.solve2(input)
// Main.res1 // part 1: 4543c154
// Main.res2 // part 2: 1050cbbd
