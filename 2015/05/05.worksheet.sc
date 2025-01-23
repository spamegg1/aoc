/*
--- Day 5: Doesn't He Have Intern-Elves For This? ---
Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:
  It contains at least three vowels (aeiou only), like
    aei, xazegov, or aeiouaeiouaeiou.
  It contains at least one letter that appears twice in a row, like
    xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  It does not contain the strings ab, cd, pq, or xy,
    even if they are part of one of the other requirements.

For example:
ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a
  double letter (...dd...), and none of the disallowed substrings.
aaa is nice because it has at least three vowels and a double letter, even
  though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

--- Part Two ---
Realizing the error of his ways, Santa has switched to a better
model of determining whether a string is naughty or nice.
None of the old rules apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:
It contains a pair of any two letters that appears at least twice in
the string without overlapping, like xyxy (xy) or aabcdefgaa (aa),
but not like aaa (aa, but it overlaps).
It contains at least one letter which repeats with exactly one
letter between them, like xyx, abcdefeghi (efe), or even aaa.

For example:
qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a
  letter that repeats with exactly one letter between them (zxz).
xxyxx is nice because it has a pair that appears twice and a letter that repeats
  with one between, even though the letters used by each rule overlap.
uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a
  single letter between them.
ieodomkazucvgmuy is naughty because it has a repeating letter with one between
  (odo), but no pair that appears twice.

How many strings are nice under these new rules?
 */
object DataDefs:
  extension (s: String)
    def containsThreeVowels: Boolean  = s.count("aeiou".contains) >= 3
    def containsDoubleLetter: Boolean = s.sliding(2).exists(p => p(0) == p(1))
    def doesNotContain: Boolean =
      !s.contains("ab") && !s.contains("cd") &&
        !s.contains("pq") && !s.contains("xy")
    def isNice1: Boolean =
      doesNotContain && containsDoubleLetter && containsThreeVowels

    def containsDoublePair: Boolean =
      val pairs = s.sliding(2).toList
      pairs.exists: p =>
        val first  = s.indexOf(p)
        val second = s.drop(first + 2).indexOf(p)
        second != -1
    def containsXyx: Boolean = s.sliding(3).exists(p => p(0) == p(2))
    def isNice2: Boolean     = containsDoublePair && containsXyx

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = lines.count(_.isNice1)
  def solve2(lines: Seq[String]) = lines.count(_.isNice2)

object Testing:
  lazy val dir     = os.pwd / "2015" / "05"
  lazy val lines1  = os.read.lines(dir / "05.test.input.txt")
  lazy val lines2  = os.read.lines(dir / "05.test.input.2.txt")
  lazy val result1 = Solving.solve1(lines1)
  lazy val result2 = Solving.solve2(lines2)
// Testing.result1 // part 1: 2
// Testing.result2 // part 2: 2

object Main:
  lazy val file    = os.pwd / "2015" / "05" / "05.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 255
// Main.result2 // part 2: 55
