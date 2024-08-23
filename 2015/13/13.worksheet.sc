/*
--- Day 13: Knights of the Dinner Table ---
In years past, the holiday feast with your family hasn't gone so well.
Not everyone gets along! This year, you resolve, will be different.
You're going to find the optimal seating arrangement and avoid all
those awkward conversations.

You start by writing up a list of everyone invited and the amount their
happiness would increase or decrease if they were to find themselves
sitting next to each other person. You have a circular table that will
be just big enough to fit everyone comfortably, and so each person will
have exactly two neighbors.

For example, suppose you have only four attendees planned, and you calculate
their potential happiness as follows:

Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.

Then, if you seat Alice next to David, Alice would lose 2 happiness units
(because David talks so much), but David would gain 46 happiness units
(because Alice is such a good listener), for a total change of 44.

If you continue around the table, you could then seat Bob next to Alice
(Bob gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob
(Carol gains 60, Bob loses 7) and David (Carol gains 55, David gains 41).
The arrangement looks like this:

     +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
     -7  +83

After trying every other seating arrangement in this hypothetical scenario,
you find that this one is the most optimal, with a total change in happiness of 330.

What is the total change in happiness for the
optimal seating arrangement of the actual guest list?

--- Part Two ---
In all the commotion, you realize that you forgot to seat yourself.
At this point, you're pretty apathetic toward the whole thing,
and your happiness wouldn't really go up or down regardless of
who you sit next to. You assume everyone else would be just as
ambivalent about sitting next to you, too.

So, add yourself to the list, and give all happiness relationships
that involve you a score of 0.

What is the total change in happiness for the optimal seating
arrangement that actually includes yourself?
 */
object Parsing:
  private def parseLine(line: String): (Seq[String], Long) = line match
    case s"$p1 would gain $score happiness units by sitting next to $p2." =>
      Seq(p1, p2) -> score.toLong
    case s"$p1 would lose $score happiness units by sitting next to $p2." =>
      Seq(p1, p2) -> -score.toLong

  def parseGuests(lines: Seq[String]) = lines.map(_.split(" ").head).distinct
  def parseHappiness(lines: Seq[String]) = lines.map(parseLine).toMap.withDefaultValue(0L)

object Solving:
  private def totalHappiness(levels: Map[Seq[String], Long])(guests: Seq[String]) =
    guests
      .sliding(2)
      .map(pair => levels(pair) + levels(pair.reverse))
      .sum

  private def solve(guests: Seq[String])(lines: Seq[String]) =
    val seatingArrangements = guests.permutations.map(a => a.appended(a.head))
    val happinessLevels = Parsing.parseHappiness(lines)
    seatingArrangements.map(totalHappiness(happinessLevels)).max

  def solve1(lines: Seq[String]) = solve(Parsing.parseGuests(lines))(lines)
  def solve2(lines: Seq[String]) = solve(Parsing.parseGuests(lines).appended("me"))(lines)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "13.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 330
Testing.result2 // part 2: 286

object Main:
  private lazy val lines = os.read.lines(os.pwd / "13.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 733
Main.result2 // part 2: 725
