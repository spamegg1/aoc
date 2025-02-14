package aoc2017.day14

object DataDefs:
  val salt = List(17, 31, 73, 47, 23)

  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def neighbours: List[Pos] = List(
      (p.x + 1, p.y),
      (p.x - 1, p.y),
      (p.x, p.y + 1),
      (p.x, p.y - 1)
    )

  extension (reps: Seq[Int])
    def hash(initial: (Seq[Int], Int)) = reps.zipWithIndex
      .foldLeft(initial): (dataIndex, sizeSkip) =>
        val (data, index) = dataIndex
        val (size, skip)  = sizeSkip
        val next          = data.take(size).reverse ++ data.drop(size)
        val offset        = (size + skip) & 0xff
        (next.drop(offset) ++ next.take(offset), (index - offset) & 0xff)

  extension (s: String)
    def knotHash: Seq[Int] =
      val sizes         = s.map(_.toInt) ++ salt
      val repeats       = Seq.fill(64)(sizes).flatten
      val initial       = (Seq.range(0, 256), 0)
      val (data, index) = repeats.hash(initial)
      (data.drop(index) ++ data.take(index))
        .grouped(16)
        .map(_.reduce(_ ^ _))
        .toSeq

    def toBinary: Seq[String] = (0 until 128).map: index =>
      s"$s-$index".knotHash
        .map: int =>
          "%08d".format(int.toBinaryString.toInt)
        .mkString

object Solving:
  import DataDefs.*

  def solve1(line: String) = line.toBinary.map(_.count(_ == '1')).sum

  def solve2(line: String) =
    val grid: Seq[Pos] = line.toBinary.zipWithIndex
      .flatMap: (row, y) =>
        for x <- 0 until 128 if row(x) == '1' yield (x, y)

    val cliques = grid.foldLeft(List.empty[List[Pos]]): (groups, pos) =>
      val (other, linked) = groups.partition(_.intersect(pos.neighbours).size == 0)
      (pos :: linked.flatten) :: other

    cliques.size

object Test:
  lazy val res1 = Solving.solve1("flqrgnkx")
  lazy val res2 = Solving.solve2("flqrgnkx")

object Main:
  lazy val res1 = Solving.solve1("nbysizxe")
  lazy val res2 = Solving.solve2("nbysizxe")

@main
def run: Unit =
  println(Test.res1) // part 1: 8108
  println(Test.res2) // part 2: 1242
  println(Main.res1) // part 1: 8216
  println(Main.res2) // part 2: 1139
