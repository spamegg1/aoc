/*
--- Day 4: The Ideal Stocking Stuffer ---
Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins, you
must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
that produces such a hash.

For example:
If your secret key is abcdef, the answer is 609043, because the MD5 hash of
abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such
number to do so.
If your secret key is pqrstuv, the lowest number it combines with to make an MD5
hash starting with five zeroes is 1048970; that is, the MD5 hash of
pqrstuv1048970 looks like 000006136ef....

--- Part Two ---
Now find one that starts with six zeroes.
 */
import org.apache.commons.codec.digest.DigestUtils.md5Hex

object Solving:
  def solve(zeroes: Int)(line: String) =
    var number = 1L
    while !md5Hex(s"$line$number").startsWith("0" * zeroes) do number += 1L
    number

object Testing:
  private lazy val lines = Seq("abcdef", "pqrstuv")
  lazy val result1 = lines map Solving.solve(5)
// Testing.result1 // part 1: 609043, 1048970

object Main:
  lazy val result1 = Solving.solve(5)("yzbqklnj")
  lazy val result2 = Solving.solve(6)("yzbqklnj")
// Main.result1 // part 1: 282749
// Main.result2 // part 2: 9962624
