/*
--- Day 22: Monkey Market ---
As you're all teleported deep into the jungle,
a monkey steals The Historians' device!
You'll need get it back while The Historians are looking for the Chief.

The monkey that stole the device seems willing to trade it,
but only in exchange for an absurd number of bananas.
Your only option is to buy bananas on the Monkey Exchange Market.

You aren't sure how the Monkey Exchange Market works,
but one of The Historians senses trouble and comes over to help.
Apparently, they've been studying these monkeys for a while and
have deciphered their secrets.

Today, the Market is full of monkeys buying good hiding spots.
Fortunately, because of the time you recently spent in this jungle,
you know lots of good hiding spots you can sell!
If you sell enough hiding spots,
you should be able to get enough bananas to buy the device back.

On the Market, the buyers seem to use random prices,
but their prices are actually only pseudorandom!
If you know the secret of how they pick their prices,
you can wait for the perfect time to sell.

The part about secrets is literal, the Historian explains.
Each buyer produces a pseudorandom sequence of secret numbers
where each secret is derived from the previous.

In particular, each buyer's secret number evolves into
the next secret number in the sequence via the following process:
  Calculate the result of multiplying the secret number by 64.
    Then, mix this result into the secret number.
    Finally, prune the secret number.
  Calculate the result of dividing the secret number by 32.
    Round the result down to the nearest integer.
    Then, mix this result into the secret number.
    Finally, prune the secret number.
  Calculate the result of multiplying the secret number by 2048.
    Then, mix this result into the secret number.
    Finally, prune the secret number.

Each step of the above process involves mixing and pruning:
    To mix a value into the secret number,
      calculate the bitwise XOR of the given value and the secret number.
      Then, the secret number becomes the result of that operation.
      (If the secret number is 42 and you were to mix 15 into the secret number,
      the secret number would become 37.)
    To prune the secret number,
      calculate the value of the secret number modulo 16777216.
      Then, the secret number becomes the result of that operation.
      (If the secret number is 100000000 and you were to prune the secret number,
      the secret number would become 16113920.)

After this process completes, the buyer is left with the next secret number
in the sequence. The buyer can repeat this process as many times as
necessary to produce more secret numbers.

So, if a buyer had a secret number of 123,
that buyer's next ten secret numbers would be:

15887950
16495136
527345
704524
1553684
12683156
11100544
12249484
7753432
5908254

Each buyer uses their own secret number when choosing their price,
so it's important to be able to predict the sequence of secret numbers
for each buyer. Fortunately, the Historian's research has uncovered the
initial secret number of each buyer (your puzzle input). For example:

1
10
100
2024

This list describes the initial secret number of four different
secret-hiding-spot-buyers on the Monkey Exchange Market.
If you can simulate secret numbers from each buyer,
you'll be able to predict all of their future prices.

In a single day, buyers each have time to generate 2000 new secret numbers.
In this example, for each buyer, their initial secret number and the
2000th new secret number they would generate are:

1: 8685429
10: 4700978
100: 15273692
2024: 8667524

Adding up the 2000th new secret number for each buyer produces 37327623.

For each buyer, simulate the creation of 2000 new secret numbers.
What is the sum of the 2000th secret number generated by each buyer?

--- Part Two ---
Of course, the secret numbers aren't the prices each buyer is offering!
That would be ridiculous. Instead, the prices the buyer offers are just
the ones digit of each of their secret numbers.

So, if a buyer starts with a secret number of 123,
that buyer's first ten prices would be:

3 (from 123)
0 (from 15887950)
6 (from 16495136)
5 (etc.)
4
4
6
4
4
2

This price is the number of bananas that buyer is offering in exchange
for your information about a new hiding spot. However, you still
don't speak monkey, so you can't negotiate with the buyers directly.
The Historian speaks a little, but not enough to negotiate;
instead, he can ask another monkey to negotiate on your behalf.

Unfortunately, the monkey only knows how to decide when to sell by
looking at the changes in price. Specifically, the monkey will only
look for a specific sequence of four consecutive changes in price,
then immediately sell when it sees that sequence.

So, if a buyer starts with a secret number of 123,
that buyer's first ten secret numbers, prices,
and the associated changes would be:

     123: 3
15887950: 0 (-3)
16495136: 6 (6)
  527345: 5 (-1)
  704524: 4 (-1)
 1553684: 4 (0)
12683156: 6 (2)
11100544: 4 (-2)
12249484: 4 (0)
 7753432: 2 (-2)

Note that the first price has no associated change because
there was no previous price to compare it with.

In this short example, within just these first few prices,
the highest price will be 6, so it would be nice to give the
monkey instructions that would make it sell at that time.
The first 6 occurs after only two changes, so there's no way
to instruct the monkey to sell then, but the second 6 occurs
after the changes -1,-1,0,2. So, if you gave the monkey that
sequence of changes, it would wait until the first time it sees
that sequence and then immediately sell your hiding spot
information at the current price, winning you 6 bananas.

Each buyer only wants to buy one hiding spot, so after the hiding spot is sold,
the monkey will move on to the next buyer. If the monkey never hears that
sequence of price changes from a buyer, the monkey will never sell,
and will instead just move on to the next buyer.

Worse, you can only give the monkey a single sequence of four price changes to look for.
You can't change the sequence between buyers.

You're going to need as many bananas as possible,
so you'll need to determine which sequence of four price changes
will cause the monkey to get you the most bananas overall.
Each buyer is going to generate 2000 secret numbers after their
initial secret number, so, for each buyer, you'll have 2000 price
changes in which your sequence can occur.

Suppose the initial secret number of each buyer is:

1
2
3
2024

There are many sequences of four price changes you could tell the monkey,
but for these four buyers, the sequence that will get you the most bananas is
-2,1,-1,3. Using that sequence, the monkey will make the following sales:
  For the buyer with an initial secret number of 1,
    changes -2,1,-1,3 first occur when the price is 7.
  For the buyer with initial secret 2,
    changes -2,1,-1,3 first occur when the price is 7.
  For the buyer with initial secret 3,
    the change sequence -2,1,-1,3 does not occur in the first 2000 changes.
  For the buyer starting with 2024,
    changes -2,1,-1,3 first occur when the price is 9.

So, by asking the monkey to sell the first time each buyer's prices
go down 2, then up 1, then down 1, then up 3, you would get 23 (7 + 7 + 9) bananas!

Figure out the best sequence to tell the monkey so that by looking
for that same sequence of changes in every buyer's future prices,
you get the most bananas in total. What is the most bananas you can get?

 */
package aoc2024.day22

object Parsing:
  def parse(lines: Seq[String]) = lines.map(_.toLong)

object Solving:
  val MOD = 1 << 24

  def solve(secret: Long): Long =
    val res1 = (secret ^ (secret << 6)) % MOD
    val res2 = (res1 ^ (res1 >> 5))     % MOD
    (res2 ^ (res2 << 11)) % MOD

  def repeat(times: Int)(start: Long) =
    (1 to times).foldLeft(start)((secret, _) => solve(secret))

  def solve1(lines: Seq[String])(times: Int) = Parsing
    .parse(lines)
    .map(repeat(times))
    .sum

  def secrets(times: Int)(start: Long): List[Long] = (1 to times)
    .foldLeft(List(start))((secrets, _) => solve(secrets.head) :: secrets)
    .reverse

  def lastDigits(secrets: List[Long]) = secrets.map(_.toString.last.asDigit)
  def priceChanges(digits: List[Int]) = digits.tail.zip(digits.init).map(_ - _)

  def solve2(lines: Seq[String])(times: Int) =
    val startSecrets = Parsing.parse(lines)
    val secretsLists = startSecrets.map(secrets(times))
    val digitsLists  = secretsLists.map(lastDigits)
    val changeLists  = digitsLists.map(priceChanges)
    0L // clean this up later!

object Testing:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "22" / "22.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(2000)
  lazy val result2 = Solving.solve2(lines)(2000)

object Main:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "22" / "22.input.txt")
  lazy val result1 = Solving.solve1(lines)(2000)
  lazy val result2 = Solving.solve2(lines)(2000)

@main
def run: Unit =
  // println(Testing.result1) // part 1: 37327623
  // println(Testing.result2) // part 2: 23
  // println(Main.result1) // part 1: 14119253575
  // println(Main.result1) // part 2: 1600
  ()
