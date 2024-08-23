/*
--- Day 4: High-Entropy Passphrases ---
A new system policy has been put in place that requires all accounts to use a
passphrase instead of simply a password. A passphrase consists of a series of words
(lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.
For example:
  aa bb cc dd ee is valid.
  aa bb cc dd aa is not valid - the word aa appears more than once.
  aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle input.
How many passphrases are valid?

--- Part Two ---
For added security, yet another system policy has been put in place.
Now, a valid passphrase must contain no two words that are anagrams of each other -
that is, a passphrase is invalid if any word's letters can be rearranged
to form any other word in the passphrase.

For example:
  abcde fghij is a valid passphrase.
  abcde xyz ecdab is not valid - the letters from the third word can be
    rearranged to form the first word.
  a ab abc abd abf abj is a valid passphrase, because all letters need
    to be used when forming another word.
  iiii oiii ooii oooi oooo is valid.
  oiii ioii iioi iiio is not valid - any of these words can be
    rearranged to form any other word.

Under this new system policy, how many passphrases are valid?
 */
object DataDefs:
  case class Passphrase(words: Array[String]):
    lazy val isValid = words.distinct.size == words.size // part 1
    lazy val noAnagrams = words.view
      .map(_.sorted)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .forall(_._2 == 1)

object Parsing:
  import DataDefs.*
  def parseLine(line: String): Passphrase = Passphrase(line.split(" "))
  def parse(lines: Seq[String]): Seq[Passphrase] = lines map parseLine

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]): Int = Parsing.parse(lines).count(_.isValid)
  def solve2(lines: Seq[String]): Int = Parsing.parse(lines).count(_.noAnagrams)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "04.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 7
Testing.result2 // part 2: 5

object Main:
  private lazy val lines = os.read.lines(os.pwd / "04.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 455
Main.result2 // part 2: 186
