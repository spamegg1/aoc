/*
--- Day 10: Adapter Array ---
Patched into the aircraft's data port, you discover weather forecasts
of a massive tropical storm. Before you can figure out whether it
will impact your vacation plans, however, your device suddenly turns off!
Its battery is dead. You'll need to plug it in.

There's only one problem: the charging outlet near your seat produces
the wrong number of jolts. Always prepared, you make a list of all
of the joltage adapters in your bag.

Each of your joltage adapters is rated for a specific output joltage
(your puzzle input).
Any given adapter can take an input 1, 2, or 3 jolts lower
than its rating and still produce its rated output joltage.

In addition, your device has a built-in joltage adapter rated for 3 jolts
higher than the highest-rated adapter in your bag.
(If your adapter list were 3, 9, and 6, your device's built-in
adapter would be rated for 12 jolts.)

Treat the charging outlet near your seat as having an effective joltage rating of 0.

Since you have some time to kill, you might as well test all of your adapters.
Wouldn't want to get to your resort and realize you can't even charge your device!

If you use every adapter in your bag at once,
what is the distribution of joltage differences between the charging outlet,
the adapters, and your device?

For example, suppose that in your bag, you have
adapters with the following joltage ratings:

16
10
15
5
1
11
7
19
6
12
4

With these adapters, your device's built-in joltage adapter
would be rated for 19 + 3 = 22 jolts, 3 higher than the highest-rated adapter.

Because adapters can only connect to a source 1-3 jolts lower than its rating,
in order to use every adapter, you'd need to choose them like this:
  The charging outlet has an effective rating of 0 jolts,
    so the only adapters that could connect to it directly would need to have
    a joltage rating of 1, 2, or 3 jolts. Of these, only one you have is an
    adapter rated 1 jolt (difference of 1).
  From your 1-jolt rated adapter, the only choice is your 4-jolt rated adapter
    (difference of 3).
  From the 4-jolt rated adapter, the adapters rated 5, 6, or 7 are valid choices.
    However, in order to not skip any adapters, you have to pick the adapter
    rated 5 jolts (difference of 1).
  Similarly, the next choices would need to be the adapter rated 6 and then
    the adapter rated 7 (with difference of 1 and 1).
  The only adapter that works with the 7-jolt rated adapter is the one rated
    10 jolts (difference of 3).
  From 10, the choices are 11 or 12; choose 11 (difference of 1) and then
    12 (difference of 1).
  After 12, only valid adapter has a rating of 15 (difference of 3),
    then 16 (difference of 1), then 19 (difference of 3).
  Finally, your device's built-in adapter is always 3 higher than the
    highest adapter, so its rating is 22 jolts (always a difference of 3).

In this example, when using every adapter,
there are 7 differences of 1 jolt and 5 differences of 3 jolts.

Here is a larger example:

28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3

In this larger example, in a chain that uses all of the adapters,
there are 22 differences of 1 jolt and 10 differences of 3 jolts.

Find a chain that uses all of your adapters to connect the
charging outlet to your device's built-in adapter and count
the joltage differences between the charging outlet, the adapters,
and your device. What is the number of 1-jolt differences
multiplied by the number of 3-jolt differences?

--- Part Two ---
To completely determine whether you have enough adapters,
you'll need to figure out how many different ways they can be arranged.
Every arrangement needs to connect the charging outlet to your device.
The previous rules about when adapters can successfully connect still apply.

The first example above (the one that starts with 16, 10, 15)
supports the following arrangements:

(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 12, 15, 16, 19, (22)

(The charging outlet and your device's built-in adapter are shown in parentheses.)
Given the adapters from the first example, the total number of arrangements
that connect the charging outlet to your device is 8.

The second example above (the one that starts with 28, 33, 18)
has many arrangements. Here are a few:

(0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, (52)

(0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 49, (52)

(0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 46, 48, 49, (52)

(0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 46, 49, (52)

(0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 47, 48, 49, (52)

(0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
46, 48, 49, (52)

(0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
46, 49, (52)

(0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
47, 48, 49, (52)

(0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
47, 49, (52)

(0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
48, 49, (52)

In total, this set of adapters can connect the charging outlet to your
device in 19208 distinct arrangements.

You glance back down at your bag and try to remember why you
brought so many adapters; there must be more than a trillion
valid ways to arrange them! Surely, there must be an efficient
way to count the arrangements.

What is the total number of distinct ways you can arrange the
adapters to connect the charging outlet to your device?
 */
object DataDefs:
  type Jolt = Long
  type Jolts = Seq[Jolt]
  type Memo = collection.mutable.Map[(Jolt, Int), Jolt]

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Jolts = lines.map(_.toLong)

object Solving:
  import DataDefs.*

  private def getJolts(lines: Seq[String]): Jolts =
    val adapters = Parsing.parse(lines)
    0L +: adapters :+ (adapters.last + 3L)

  def solve1(lines: Seq[String]): Jolt =
    val jolts = getJolts(lines)
    val pairs = jolts.sliding(2).toSeq // ITERATOR BUG! It can only be used once.
    val ones = pairs.count(pair => pair.last - pair.head == 1)
    val threes = pairs.count(pair => pair.last - pair.head == 3)
    ones * threes

  private def paths(jolt: Jolt, index: Int)(using memo: Memo, jolts: Jolts): Jolt =
    memo.get((jolt, index)) match
      case Some(value) => value
      case None =>
        val slice = jolts.drop(index + 1).takeWhile(_ - jolt <= 3L)
        val jolt1 = slice.head
        val res1 = paths(jolt1, index + 1) // recur!
        memo.update((jolt1, index + 1), res1) // memoize!
        if slice.size == 1 then res1
        else
          val jolt2 = slice.tail.head
          val res2 = paths(jolt2, index + 2) // recur!
          memo.update((jolt2, index + 2), res2) // memoize!
          if slice.size == 2 then res1 + res2
          else
            val jolt3 = slice.last
            val res3 = paths(jolt3, index + 3) // recur!
            memo.update((jolt3, index + 3), res3) // memoize!
            res1 + res2 + res3

  def solve2(lines: Seq[String]) =
    given jolts: Jolts = getJolts(lines)
    given Memo = collection.mutable.Map((jolts.last, jolts.size - 1) -> 1L)
    paths(jolts.head, 0)

object Test:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "10" / "10.test.sorted.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Test.res1 // part 1: 220
// Test.res2 // part 2: 19208

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2020" / "10" / "10.sorted.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 1876
// Main.res2 // part 2: 14173478093824 // FIRST TRY!!!
