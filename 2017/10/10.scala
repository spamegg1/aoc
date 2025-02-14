package aoc2017.day10

object DataDefs:
  case class State(list: Map[Int, Int], size: Int, cur: Int, skip: Int, lens: List[Int]):
    lazy val isFinished = lens.isEmpty

    private lazy val (keys1, keys2) =
      if cur + lens.head < size then (cur until cur + lens.head, 0 until 0)
      else (cur until size, 0 until (cur + lens.head - size))

    private lazy val rev                = (keys1.map(list) ++ keys2.map(list)).reverse
    private lazy val (rev1, rev2)       = (rev.take(keys1.size), rev.drop(keys1.size))
    private lazy val (update1, update2) = (keys1.zip(rev1).toMap, keys2.zip(rev2).toMap)

    private lazy val newList = list.map: (key, value) =>
      if keys1 contains key then key -> update1(key)
      else if keys2 contains key then key -> update2(key)
      else key                            -> value

    lazy val next = copy(
      list = newList,
      cur = (cur + lens.head + skip) % size,
      skip = skip + 1,
      lens = lens.tail
    )

    lazy val sparseHash = list.toSeq.sortBy(_._1).map(_._2)

  object State:
    def apply(lens: List[Int], size: Int) =
      new State((0 until size).map(i => i -> i).toMap, size, 0, 0, lens)

object Parsing:
  import DataDefs.*
  private lazy val salt        = List(17, 31, 73, 47, 23)
  def parse(line: String)      = line.split(",").map(_.toInt).toList      // part 1
  def parseASCII(line: String) = line.map(_.toByte.toInt).toList ::: salt // part 2

object Solving:
  import DataDefs.*

  def bulkXOR(bytes: Seq[Int]) = bytes.foldLeft(0)(_ ^ _)

  def denseHash(sparse: Seq[Int]) = sparse
    .grouped(16) // 16 groups each of length 16
    .map(bulkXOR)
    .toList

  def convertBytesToHex(bytes: Seq[Int]): String = bytes
    .map(String.format("%02x", _))
    .mkString

  def repeatList(list: List[Int])(rounds: Int) =
    (0 until rounds - 1).foldLeft(list)((l, _) => l ::: list)

  def solve1(line: String)(size: Int) =
    var state = State(Parsing.parse(line), size)
    while !state.isFinished do state = state.next
    state.list(0) * state.list(1)

  def solve2(line: String)(size: Int)(rounds: Int) =
    val lens  = repeatList(Parsing.parseASCII(line))(rounds)
    var state = State(lens, size)
    while !state.isFinished do state = state.next
    convertBytesToHex(denseHash(state.sparseHash))

object Test:
  lazy val file   = os.pwd / "2017" / "10" / "10.test.input.txt"
  lazy val line   = os.read.lines(file).head
  lazy val state0 = DataDefs.State(Parsing.parse(line), 5)
  lazy val state1 = state0.next // 2 1 0 [3] 4
  lazy val state2 = state1.next // 4 3 0 [1] 2
  lazy val state3 = state2.next // 4 3 0 [1] 2
  lazy val state4 = state3.next // 3 4 2 1 [0]
  lazy val res1   = Solving.solve1(line)(5)
  lazy val res21  = Solving.solve2("1,2,3")(256)(64)
  lazy val res22  = Solving.solve2("1,2,4")(256)(64)

object Main:
  lazy val file = os.pwd / "2017" / "10" / "10.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)(256)
  lazy val res2 = Solving.solve2(line)(256)(64)

@main
def run: Unit =
  println(Test.res1)  // part 1: 12
  println(Test.res21) // part 2: 3efbe78a8d82f29979031a4aa0b16a9d
  println(Test.res22) // part 2: 63960835bcdc130f0b66d7ff4f6a5a8e
  println(Main.res1)  // part 1: 62238
  println(Main.res2)  // part 2: 2b0c9cc0449507a0db3babd57ad9e8d8
