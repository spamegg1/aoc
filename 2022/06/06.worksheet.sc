object Solving:
  def solve(size: Int)(line: String) = line
    .sliding(size)
    .indexWhere(_.distinct.size == size)
    + size
  val solve1 = solve(4)
  val solve2 = solve(14)

object Test:
  val file  = os.pwd / "2022" / "06" / "06.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = lines.map(Solving.solve1)
  val res2  = lines.map(Solving.solve2)
// Test.res1 // part 1: 7,5,6,10,11
// Test.res2 // part 2: 19,23,23,29,26

object Main:
  val file = os.pwd / "2022" / "06" / "06.input.txt"
  val line = os.read.lines(file).head
  val res1 = Solving.solve1(line)
  val res2 = Solving.solve2(line)
// Main.res1 // part 1: 1361
// Main.res2 // part 2: 3263
