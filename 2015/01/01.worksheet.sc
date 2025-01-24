object Solving:
  import util.boundary, boundary.break

  def solve1(line: String) = line.foldLeft(0): (acc, c) =>
    acc + (if c == '(' then 1 else -1)

  def solve2(line: String) =
    var index = 0
    var acc   = 0
    boundary:
      while 0 <= acc do
        val char = line(index)
        acc += (if char == '(' then 1 else -1)
        if acc < 0 then break()
        else index += 1
    index + 1

object Testing:
  lazy val file    = os.pwd / "2015" / "01" / "01.test.input.txt"
  lazy val lines   = os.read.lines(file)
  lazy val result1 = lines map Solving.solve1
// Testing.result1 // part1: 0,0,3,3,3,-1,-1,-3,-3

object Main:
  lazy val file    = os.pwd / "2015" / "01" / "01.input.txt"
  lazy val line    = os.read(file)
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part1: 74
// Main.result2 // part2: 1795
