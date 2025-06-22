object DataDefs:
  case class Lantern(fish: Map[Int, Long]):
    def next: Lantern =
      val zeros     = fish.getOrElse(0, 0L) // 0 resets to 6
      val sevens    = fish.getOrElse(7, 0L) // 7 decreases to 6
      val nextSixes = zeros + sevens        // both 0 and 7 become 6, so put them together
      val newFish = fish
        .filterNot: (k, _) =>
          k == 0 || k == 7 // get rid of 0 and 7
        .map: (timer, count) =>
          (timer - 1, count) // all others decrease by 1
      val sixesAndEights = collection.mutable.Map[Int, Long]()
      if nextSixes > 0 then sixesAndEights += 6 -> nextSixes
      if zeros > 0 then sixesAndEights += 8     -> zeros
      Lantern(newFish ++ sixesAndEights)

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]) = Lantern:
    lines.head
      .split(",")
      .map(_.toInt)
      .groupMapReduce(identity)(_ => 1L)(_ + _)

object Solving:
  def solve(lines: Seq[String])(days: Int): Long =
    var lantern = Parsing.parse(lines)
    for day <- 0 until days do lantern = lantern.next
    lantern.fish.values.sum

object Test:
  val file  = os.pwd / "2021" / "06" / "06.test.input.txt"
  val lines = os.read.lines(file)
  val res11 = Solving.solve(lines)(18)
  val res12 = Solving.solve(lines)(80)
  val res2  = Solving.solve(lines)(256)
// Test.res11 // part 1: 26
// Test.res12 // part 1: 5934
// Test.res2 // part 2: 26984457539

object Main:
  val file  = os.pwd / "2021" / "06" / "06.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve(lines)(80)
  val res2  = Solving.solve(lines)(256)
// Main.res1 // part 1: 372984
// Main.res2 // part 2: 1681503251694
