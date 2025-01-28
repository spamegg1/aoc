/*
--- Day 16: Ticket Translation ---
As you're walking to yet another connecting flight,
you realize that one of the legs of your re-routed
trip coming up is on a high-speed train.
However, the train ticket you were given is in a language
you don't understand. You should probably figure out what
it says before you get to the train station after the next flight.

Unfortunately, you can't actually read the words on the ticket.
You can, however, read the numbers, and so you figure out the
fields these tickets must have and
the valid ranges for values in those fields.

You collect the rules for ticket fields, the numbers on your ticket,
nd the numbers on other nearby tickets for the same train service
(via the airport security cameras) together into a single document
you can reference (your puzzle input).

The rules for ticket fields specify a list of fields that exist
somewhere on the ticket and the valid ranges of values for each field.
For example, a rule like class: 1-3 or 5-7 means that one of
the fields in every ticket is named class and can be any value
in the ranges 1-3 or 5-7 (inclusive, such that 3 and 5
are both valid in this field, but 4 is not).

Each ticket is represented by a single line of comma-separated values.
The values are the numbers on the ticket in the order they appear;
every ticket has the same format. For example, consider this ticket:

.--------------------------------------------------------.
| ????: 101    ?????: 102   ??????????: 103     ???: 104 |
|                                                        |
| ??: 301  ??: 302             ???????: 303      ??????? |
| ??: 401  ??: 402           ???? ????: 403    ????????? |
'--------------------------------------------------------'

Here, ? represents text in a language you don't understand.
This ticket might be represented as
101,102,103,104,301,302,303,401,402,403;
of course, the actual train tickets you're
looking at are much more complicated.
In any case, you've extracted just the numbers in such a way that
the first number is always the same specific field, the second
number is always a different specific field, and so on -
you just don't know what each position actually means!

Start by determining which tickets are completely invalid;
these are tickets that contain values which aren't
valid for any field. Ignore your ticket for now.

For example, suppose you have the following notes:

class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12

It doesn't matter which position corresponds to which field;
you can identify invalid nearby tickets by considering only whether
tickets contain values that are not valid for any field.
In this example, the values on the first nearby ticket are
all valid for at least one field. This is not true of the
other three nearby tickets: the values 4, 55, and 12 are are
not valid for any field. Adding together all of the invalid
values produces your ticket scanning error rate: 4 + 55 + 12 = 71.

Consider the validity of the nearby tickets you scanned.
What is your ticket scanning error rate?

--- Part Two ---
Now that you've identified which tickets contain invalid values,
discard those tickets entirely. Use the remaining
valid tickets to determine which field is which.

Using the valid ranges for each field,
determine what order the fields appear on the tickets.
The order is consistent between all tickets:
  if seat is the third field,
  it is the third field on every ticket,
  including your ticket.

For example, suppose you have the following notes:

class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9

Based on the nearby tickets in the above example,
the first position must be row,
the second position must be class,
and the third position must be seat;
you can conclude that in your ticket,
class is 12, row is 11, and seat is 13.

Once you work out which field is which,
look for the six fields on your ticket
that start with the word departure.
What do you get if you multiply those six values together?
 */
object DataDefs:
  type Name = String
  type Names = Set[Name]
  extension (sets: Seq[Names])
    def intersectAll = sets.tail.foldLeft(sets.head)(_ intersect _)

  case class Field(name: Name, ranges: Seq[Range]):
    def canContain(value: Int) = ranges.exists(_.contains(value))

  type Fields = Seq[Field] // part 2
  extension (fields: Fields)
    def canContain(value: Int): Names = fields
      .filter(_.canContain(value))
      .map(_.name)
      .toSet

  type Ticket = Seq[Int]
  type Tickets = Seq[Ticket]
  extension (nearbyTickets: Tickets)
    def transpose: Tickets = // part 2
      for col <- 0 until nearbyTickets.head.size
      yield for row <- 0 until nearbyTickets.size
      yield nearbyTickets(row)(col)

object Parsing:
  import DataDefs.*

  def parseTicket(line: String): Ticket =
    line.split(",").map(_.toInt).toSeq

  def parseField(line: String): Field = line match
    case s"$f: $lo1-$hi1 or $lo2-$hi2" =>
      Field(f, Seq(lo1.toInt to hi1.toInt, lo2.toInt to hi2.toInt))

  def splitAll(lines: String) = lines
    .split("\n\n")
    .map(_.split("\n").toSeq)
    .toSeq

  def parse(lines: String): (Fields, Ticket, Tickets) =
    val Seq(rawFields, mine, nearby) = splitAll(lines)
    val fields = rawFields.map(parseField)
    val myTicket = parseTicket(mine.last)
    val nearbyTickets = nearby.tail.map(parseTicket)
    (fields, myTicket, nearbyTickets)

object Solving:
  import DataDefs.*

  def solve1(lines: String): Int =
    val (fields, _, nearbyTickets) = Parsing.parse(lines)
    val allRanges = fields.flatMap(_.ranges)
    nearbyTickets
      .flatMap(_.filterNot(i => allRanges.exists(_.contains(i))))
      .sum

  // interesting example of "simultaneously folding and mapping"
  // or, equivalently, "mutating while iterating"
  extension (fields: Seq[(Names, Int)])
    def foldMap: Seq[(Name, Int)] = // part 2
      var solved: Names = Set()
      var res: Seq[(Name, Int)] = Seq()
      for i <- 0 until fields.size do
        val (names, index) = fields(i)
        val newSolved = names diff solved
        solved += newSolved.head
        res :+= (newSolved.head, index)
      res

  def solve2(lines: String): Long =
    val (fields, myTicket, nearbyTickets) = Parsing.parse(lines)
    val allRanges = fields.flatMap(_.ranges)
    nearbyTickets
      .filter(_.forall(i => allRanges.exists(_.contains(i)))) // valid
      .transpose // get columns of nearby tickets: field1, field2, ...
      .map(_.map(fields.canContain).intersectAll) // possible names
      .zipWithIndex // we need to keep track of the order
      .sortBy(_._1.size) // start with smallest possible names
      .foldMap // resolve all fields down to a single name
      .sortBy(_._2) // sort by index, to match with my ticket
      .map(_._1) // get only the field names
      .zip(myTicket) // pair them up with my ticket values
      .filter(_._1.startsWith("departure")) // comment out for test
      .map(_._2.toLong) // we need long to avoid overflow
      .product

object Test:
  private lazy val lines1 = os.read(os.pwd / "2020" / "16" / "16.test.input.txt")
  private lazy val lines2 = os.read(os.pwd / "2020" / "16" / "16.test.input.2.txt")
  lazy val res1 = Solving.solve1(lines1)
  lazy val res2 = Solving.solve2(lines2)
// Test.res1 // part 1: 71
// Test.res2 // part 2: row:11,class:12,seat:13 = 1716

object Main:
  private lazy val lines = os.read(os.pwd / "2020" / "16" / "16.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
// Main.res1 // part 1: 26009
// Main.res2 // part 2: 589685618167
