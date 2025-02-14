object DataDefs:
  type Bank = Int
  case class State(
      banks: Seq[Bank],
      index: Int,
      max: Bank,
      size: Int,
      seen: Map[Seq[Bank], Int],
      cycle: Int
  ):
    def next: State =
      val newSeen               = seen + (banks -> cycle)
      val (quotient, remainder) = (max / size, max % size)
      val removeMax             = banks.updated(index, 0)

      var addOneToBanks = removeMax
      var i             = index + 1
      var counter       = 0
      while counter < remainder do
        if i >= size then i = 0
        val currentBank = addOneToBanks(i)
        addOneToBanks = addOneToBanks.updated(i, currentBank + 1)
        counter += 1
        i += 1

      val newBanks = addOneToBanks.map(_ + quotient)
      val newMax   = newBanks.max
      val newIndex = newBanks.indexOf(newMax)
      State(newBanks, newIndex, newMax, size, newSeen, cycle + 1)

object Parsing:
  import DataDefs.*
  def parse(line: String): Seq[Bank] = line.split(" ").map(_.toInt).toSeq

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def helper1(state: State): State = state.seen.get(state.banks) match
    case None        => helper1(state.next)
    case Some(value) => state

  def start(line: String) =
    val banks = Parsing.parse(line)
    val max   = banks.max
    val index = banks.indexOf(max)
    State(banks, index, max, banks.size, Map[Seq[Bank], Int](), 0)

  def solve1(line: String): Int = helper1(start(line)).cycle

  @annotation.tailrec
  def helper2(state: State)(seen: Seq[Bank])(cycle: Int): Int =
    if state.banks == seen then cycle
    else helper2(state.next)(seen)(cycle + 1)

  def solve2(line: String): Int =
    val firstSeen = helper1(start(line))
    helper2(firstSeen.next)(firstSeen.banks)(1)

object Test:
  lazy val file = os.pwd / "2017" / "06" / "06.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Test.res1 // part 1: 5
// Test.res2 // part 2: 4

object Main:
  lazy val file = os.pwd / "2017" / "06" / "06.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 12841
// Main.res2 // part 2: 8038
