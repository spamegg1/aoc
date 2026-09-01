object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String])(key: Long) = lines
    .map(_.toInt * key)
    .zipWithIndex

object Solving:
  import DataDefs.*

  def decrypt(rounds: Int)(input: Seq[(Long, Int)]): Long =
    val mixed = collection.mutable.ArrayBuffer.from(input)
    for _ <- 1 to rounds do
      for index <- input.indices do
        val from               = mixed.indexWhere(_._2 == index)
        val pair @ (number, _) = mixed.remove(from)
        val remainder          = (number                         % mixed.size).toInt
        val to                 = (from + remainder + mixed.size) % mixed.size
        mixed.insert(to, pair)
    val start = mixed.indexWhere(_._1 == 0)
    (1 to 3)
      .map: offset =>
        mixed((start + 1000 * offset) % mixed.size)._1
      .sum

  def solve(rounds: Int, key: Long)(lines: Seq[String]) =
    decrypt(rounds)(Parsing.parse(lines)(key))

  val solve1 = solve(1, 1L)
  val solve2 = solve(10, 811589153L)

object Test:
  val file  = os.pwd / "2022" / "20" / "20.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 3
// Test.res2 // part 2: 1623178306

object Main:
  val file  = os.pwd / "2022" / "20" / "20.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 5962
// Main.res2 // part 2: 9862431387256
