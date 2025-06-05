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
    def transp: Tickets = // part 2
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
      .transp // get columns of nearby tickets: field1, field2, ...
      .map(_.map(fields.canContain).intersectAll) // possible names
      .zipWithIndex // we need to keep track of the order
      .sortBy(_._1.size) // start with smallest possible names
      .foldMap // resolve all fields down to a single name
      .sortBy(_._2) // sort by index, to match with my ticket
      .map(_._1) // get only the field names
      .zip(myTicket) // pair them up with my ticket values
      .filter(_._1.startsWith("departure")) // comment out for test part 2
      .map(_._2.toLong) // we need long to avoid overflow
      .product

object Test:
  lazy val path = os.pwd / "2020" / "16"
  lazy val file1 = path / "16.test.input.1.txt"
  lazy val file2 = path / "16.test.input.2.txt"
  lazy val lines1 = os.read(file1)
  lazy val lines2 = os.read(file2)
  lazy val res1 = Solving.solve1(lines1)
  lazy val res2 = Solving.solve2(lines2)
Test.res1 // part 1: 71
Test.res2 // part 2: row:11,class:12,seat:13 = 1716

object Main:
  lazy val file = os.pwd / "2020" / "16" / "16.input.txt"
  lazy val lines = os.read(file)
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)
Main.res1 // part 1: 26009
Main.res2 // part 2: 589685618167
