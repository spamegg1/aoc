/*
--- Day 2: Password Philosophy ---
Your flight departs in a few days from the coastal airport; the easiest way down
to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
"Something's wrong with our computers; we can't log in!" You ask if you can take
a look.

Their password database seems to be a little corrupted: some of the passwords
wouldn't have been allowed by the Official Toboggan Corporate Policy that was in
effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of
passwords (according to the corrupted database) and the corporate policy when
that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy
indicates the lowest and highest number of times a given letter must appear for
the password to be valid. For example, 1-3 a means that the password must
contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not;
it contains no instances of b, but needs at least 1. The first and third
passwords are valid: they contain one a or nine c, both within the limits of
their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---
While it appears you validated the passwords correctly, they don't seem to be
what the Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the
password policy rules from his old job at the sled rental place down the street!
The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the
first character, 2 means the second character, and so on. (Be careful; Toboggan
Corporate Policies have no concept of "index zero"!) Exactly one of these
positions must contain the given letter. Other occurrences of the letter are
irrelevant for the purposes of policy enforcement.

Given the same example list from above:

    1-3 a: abcde is valid: position 1 contains a and position 3 does not.
    1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
    2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

How many passwords are valid according to the new interpretation of the
policies?
 */
object DataDefs:
  case class Policy(low: Int, hi: Int, char: Char)
  case class Password(policy: Policy, password: String):
    lazy val isValid1 =
      val count = password.count(_ == policy.char)
      policy.low <= count && count <= policy.hi
    lazy val isValid2 =
      Seq(password(policy.low - 1), password(policy.hi - 1))
        .count(_ == policy.char) == 1

object Parsing:
  import DataDefs.*

  def parsePassword(line: String): Password = line match
    case s"$low-$hi $char: $password" =>
      Password(Policy(low.toInt, hi.toInt, char(0)), password)

  def parse(lines: Seq[String]): Seq[Password] = lines.map(parsePassword)

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def solve1(lines: Seq[String]): Long = Parsing
    .parse(lines)
    .count(_.isValid1)

  def solve2(lines: Seq[String]): Long = Parsing
    .parse(lines)
    .count(_.isValid2)

object Test:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "02" / "02.test.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Test.res1 // part 1: 2
// Test.res2 // part 2: 1

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "02" / "02.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 416
// Main.res2 // part 2: 688
