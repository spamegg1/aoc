object Parsing:
  def parse(line: String)(size: Int) =
    val (prefix, suffix) = (line.take(size), line.drop(size))
    val shifted          = suffix ++ prefix
    line.map(_.asDigit).zip(shifted.map(_.asDigit))

object Solving:
  def matches(pairs: IndexedSeq[(Int, Int)]) =
    pairs.filter(pair => pair._1 == pair._2).map(_._1)

  def solve(size: Int)(line: String): Int =
    val pairs = Parsing.parse(line)(size)
    matches(pairs).sum

object Test:
  lazy val ins1 = Seq("1122", "1111", "1234", "91212129")
  lazy val ins2 = Seq("1212", "1221", "123425", "123123", "12131415")
  lazy val res1 = ins1 map Solving.solve(1)
  lazy val res2 = ins2.map(inp => Solving.solve(inp.size / 2)(inp))
// Test.res1 // part 1: 3,4,0,9
// Test.res2 // part 2: 6,0,4,12,4

object Main:
  lazy val file = os.pwd / "2017" / "01" / "01.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve(1)(line)
  lazy val res2 = Solving.solve(line.size / 2)(line)
// Main.res1 // part 1: 1343
// Main.res2 // part 2: 1274
