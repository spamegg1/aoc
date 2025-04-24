import collection.mutable.{Map => MMap}

object DataDefs:
  type Jolt  = Long
  type Jolts = Seq[Jolt]
  type Memo  = MMap[(Jolt, Int), Jolt]

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): Jolts = lines.map(_.toLong)

object Solving:
  import DataDefs.*

  private def getJolts(lines: Seq[String]): Jolts =
    val adapters = Parsing.parse(lines)
    0L +: adapters :+ (adapters.last + 3L)

  def solve1(lines: Seq[String]): Jolt =
    val jolts  = getJolts(lines)
    val pairs  = jolts.sliding(2).toSeq // ITERATOR BUG! It can only be used once.
    val ones   = pairs.count(pair => pair.last - pair.head == 1)
    val threes = pairs.count(pair => pair.last - pair.head == 3)
    ones * threes

  private def paths(jolt: Jolt, index: Int)(using memo: Memo, jolts: Jolts): Jolt =
    memo.get((jolt, index)) match
      case Some(value) => value
      case None =>
        val slice = jolts.drop(index + 1).takeWhile(_ - jolt <= 3L)
        val jolt1 = slice.head
        val res1  = paths(jolt1, index + 1) // recur!
        memo.update((jolt1, index + 1), res1) // memoize!
        if slice.size == 1 then res1
        else
          val jolt2 = slice.tail.head
          val res2  = paths(jolt2, index + 2) // recur!
          memo.update((jolt2, index + 2), res2) // memoize!
          if slice.size == 2 then res1 + res2
          else
            val jolt3 = slice.last
            val res3  = paths(jolt3, index + 3) // recur!
            memo.update((jolt3, index + 3), res3) // memoize!
            res1 + res2 + res3

  def solve2(lines: Seq[String]) =
    given jolts: Jolts = getJolts(lines)
    given Memo         = MMap((jolts.last, jolts.size - 1) -> 1L)
    paths(jolts.head, 0)

object Test:
  lazy val file  = os.pwd / "2020" / "10" / "10.test.sorted.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 220
// Test.res2 // part 2: 19208

object Main:
  lazy val file  = os.pwd / "2020" / "10" / "10.sorted.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1876
// Main.res2 // part 2: 14173478093824 // FIRST TRY!!!
