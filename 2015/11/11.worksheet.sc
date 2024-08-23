/*
--- Day 11: Corporate Policy ---
Santa's previous password expired, and he needs help choosing a new one.

To help him remember his new password after the old one expires,
Santa has devised a method of coming up with a password based on
the previous one. Corporate policy dictates that passwords must
be exactly eight lowercase letters (for security reasons),
so he finds his new password by incrementing his old password
string repeatedly until it is valid.

Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
Increase the rightmost letter one step; if it was z, it wraps around to a,
and repeat with the next letter to the left until one doesn't wrap around.

Unfortunately for Santa, a new Security-Elf recently started,
and he has imposed some additional password requirements:
  Passwords must include one increasing straight of at least three letters,
    like abc, bcd, cde, and so on, up to xyz.
    They cannot skip letters; abd doesn't count.
  Passwords may not contain the letters i, o, or l, as these letters can be mistaken
    for other characters and are therefore confusing.
  Passwords must contain at least two different, non-overlapping pairs of letters,
    like aa, bb, or zz.

For example:
  hijklmmn meets the first requirement (because it contains the straight hij)
    but fails the second requirement (because it contains i and l).
  abbceffg meets the third requirement (because it repeats bb and ff)
    but fails the first requirement.
  abbcegjk fails the third requirement, because it only has one double letter (bb).
  The next password after abcdefgh is abcdffaa.
  The next password after ghijklmn is ghjaabcc, because you eventually skip all
    the passwords that start with ghi..., since i is not allowed.

Given Santa's current password (your puzzle input), what should his next password be?

--- Part Two ---
Santa's password expired again. What's the next one?
 */
import scala.compiletime.ops.double
object DataDefs:
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  val excluded = "ilo"
  val pairs = alphabet.map(_.toString * 2).filterNot(_.isExcluded)
  val threes = alphabet.sliding(3).filterNot(isExcluded).toSeq

  extension (s: String)
    def isExcluded = s.exists(excluded.contains(_))
    def increment: String =
      if s.isEmpty then s
      else if s.endsWith("z") then s.init.increment + "a"
      else s.init.appended((s.last + 1).toChar)
    def req1 = threes.exists(s.contains(_))
    def req2 = !s.isExcluded
    def req3 = s.sliding(2).toSeq.intersect(pairs).size >= 2
    def isValid = s.req1 && s.req2 && s.req3

object Solving:
  import DataDefs.*

  def solve(pass: String) =
    var nextPass = pass
    while !nextPass.isValid do nextPass = nextPass.increment
    nextPass

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "11.test.input.txt")
  lazy val result1 = lines map Solving.solve
Testing.result1 // part 1: abcdffaa, ghjaabcc

object Main:
  import DataDefs.*
  private lazy val line = "cqjxjnds"
  lazy val result1 = Solving.solve(line)
  lazy val result2 = Solving.solve(result1.increment)
Main.result1 // part 1: cqjxxyzz
Main.result2 // part 2: cqkaabcc
