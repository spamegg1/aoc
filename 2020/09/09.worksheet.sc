object Parsing:
  def parse(lines: Seq[String]) = lines map (_.toLong)

object Solving:
  def property(number: Long)(slice: Seq[Long]): Boolean =
    !slice.combinations(2).map(_.sum).contains(number)

  def solve1(lines: Seq[String])(sliceSize: Int) =
    val numbers = Parsing.parse(lines)
    numbers.zipWithIndex
      .find: (number, index) =>
        index > sliceSize && property(number)(numbers.slice(index - sliceSize, index))
      .get

  // I will manually try different contiguous block sizes, one by one.
  def solve2(lines: Seq[String])(sliceSize: Int)(blockSize: Int) =
    val numbers          = Parsing.parse(lines)
    val (invalid, index) = solve1(lines)(sliceSize)
    val sums = numbers
      .slice(0, index)
      .sliding(blockSize)
      .map(combo => combo.sum -> combo)
      .toMap
    val seq = sums.getOrElse(invalid, Seq(0L, 0L))
    seq.min + seq.max // 0 if this block size did not work.

object Test:
  lazy val file  = os.pwd / "2020" / "09" / "09.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(5)
  lazy val res2  = Solving.solve2(lines)(5)(4) // tried: 3,4
// Test.res1 // part 1: 127,14
// Test.res2 // part 2: 15,25,47,40 => 62

object Main:
  lazy val file  = os.pwd / "2020" / "09" / "09.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(25)
  lazy val res2  = Solving.solve2(lines)(25)(17) // tried: 3,4,5,...,15,16,17
// Main.res1 // part 1: 530627549,610
// Main.res2 // part 2: 77730285
