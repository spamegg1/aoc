import ujson.*

object Solving:
  def findNumbers(pred: Value => Boolean)(json: Value): Double = json match
    case Obj(value) =>
      if pred(value) then 0.0
      else value.values.map(findNumbers(pred)).sum
    case Arr(value) => value.map(findNumbers(pred)).sum
    case Num(value) => value
    case _          => 0.0

  def ignoreRed(json: Value): Boolean = json match
    case Obj(value) =>
      value.values.exists: v =>
        v.match
          case Str(value) => value == "red"
          case _          => false
    case _ => false

  def solve1(line: String) = findNumbers(_ => false)(read(line))
  def solve2(line: String) = findNumbers(ignoreRed)(read(line))

object Test:
  lazy val file  = os.pwd / "2015" / "12" / "12.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2
// Test.res1 // part 1: 15,6,6,6,3,3,0,0,0,0
// Test.res2 // part 2:  0,6,6,4,3,3,0,0,0,0

object Main:
  lazy val file = os.pwd / "2015" / "12" / "12.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 111754
// Main.res2 // part 2: 65402
