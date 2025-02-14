object DataDefs:
  case class State(in: List[Char], nest: Int, garbage: Boolean, score: Int, count: Int):
    def next =
      if !garbage then
        in.head match
          case '{' => copy(in = in.tail, nest = nest + 1)
          case '}' => copy(in = in.tail, nest = nest - 1, score = score + nest)
          case '<' => copy(in = in.tail, garbage = true)
          case '!' => copy(in = in.tail.tail)
          case _   => copy(in = in.tail)
      else
        in.head match
          case '!' => copy(in = in.tail.tail)
          case '>' => copy(in = in.tail, garbage = false)
          case _   => copy(in = in.tail, count = count + 1)

object Solving:
  import DataDefs.*

  def solve(line: String) =
    var state = State(line.toList, 0, false, 0, 0)
    while state.in.nonEmpty do state = state.next
    state

  def solve1(line: String) = solve(line).score
  def solve2(line: String) = solve(line).count

object Test:
  lazy val file  = os.pwd / "2017" / "09" / "09.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2
// Test.res1 // part 1: 1,6,5,16,1,9,9, 3, 1,9, 3, 0
// Test.res2 // part 2: 0,0,0, 0,4,8,0,17,10,4,13,17

object Main:
  lazy val file = os.pwd / "2017" / "09" / "09.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 21037
// Main.res2 // part 2: 9495
