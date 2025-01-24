import org.apache.commons.codec.digest.DigestUtils.md5Hex

object Solving:
  def solve(zeroes: Int)(line: String) =
    var number = 1L
    while !md5Hex(s"$line$number").startsWith("0" * zeroes) do number += 1L
    number

object Test:
  lazy val lines = Seq("abcdef", "pqrstuv")
  lazy val res1  = lines map Solving.solve(5)
// Test.res1 // part 1: 609043, 1048970

object Main:
  lazy val res1 = Solving.solve(5)("yzbqklnj")
  lazy val res2 = Solving.solve(6)("yzbqklnj")
// Main.res1 // part 1: 282749
// Main.res2 // part 2: 9962624
