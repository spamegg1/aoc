object DataDefs:
  val Threshold = 12

  case class Beacon(x: Int, y: Int, z: Int):
    def +(other: Beacon): Beacon = Beacon(x + other.x, y + other.y, z + other.z)
    def -(other: Beacon): Beacon = Beacon(x - other.x, y - other.y, z - other.z)
    def manhattan(other: Beacon): Int =
      (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
    def permutations: Seq[Beacon] = Seq(
      Beacon(x, y, z),
      Beacon(y, z, x),
      Beacon(z, x, y),
      Beacon(-x, z, y),
      Beacon(z, y, -x),
      Beacon(y, -x, z),
      Beacon(x, z, -y),
      Beacon(z, -y, x),
      Beacon(-y, x, z),
      Beacon(x, -z, y),
      Beacon(-z, y, x),
      Beacon(y, x, -z),
      Beacon(-x, -y, z),
      Beacon(-y, z, -x),
      Beacon(z, -x, -y),
      Beacon(-x, y, -z),
      Beacon(y, -z, -x),
      Beacon(-z, -x, y),
      Beacon(x, -y, -z),
      Beacon(-y, -z, x),
      Beacon(-z, x, -y),
      Beacon(-x, -z, -y),
      Beacon(-z, -y, -x),
      Beacon(-y, -x, -z)
    )
  end Beacon

  case class Scanner(beacons: Seq[Beacon]):
    val deltas =
      (for
        first  <- beacons
        second <- beacons
      yield first - second).toSet
    def permutations = beacons.map(_.permutations).transpose.map(Scanner(_))
end DataDefs

object Parsing:
  import DataDefs.*

  def parse(lines: String): Seq[Scanner] = lines
    .split("\n\n")
    .toSeq
    .map: block =>
      Scanner:
        block.trim
          .split("\n")
          .tail
          .map(_.trim.split(",").map(_.toInt))
          .map(a => Beacon(a(0), a(1), a(2)))
          .toSeq
end Parsing

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def condition(s: Scanner, bs: Set[Beacon], b1: Beacon, b2: Beacon): Boolean =
    s.beacons
      .map(_ + b1 - b2)
      .toSet
      .intersect(bs)
      .size >= Threshold

  def findMatch(first: Scanner, candidate: Scanner): Option[(Scanner, Beacon)] =
    val bs = first.beacons.toSet
    boundary:
      for
        second <- candidate.permutations
        if second.deltas.intersect(first.deltas).size > 12 * 11
      do
        for
          b1 <- first.beacons
          b2 <- second.beacons
          if condition(second, bs, b1, b2)
        do break(Some((second, b1 - b2)))
      None

  @annotation.tailrec
  def search(
      beacons: Set[Beacon],
      offsets: Seq[Beacon],
      todo: Map[Scanner, Beacon],
      remaining: Seq[Scanner]
  ): (Set[Beacon], Seq[Beacon]) =
    if todo.isEmpty then (beacons, offsets)
    else
      val (current, currentOffset) = todo.head
      val nextBeacons              = beacons ++ current.beacons.map(_ + currentOffset)
      val nextOffsets              = offsets.appended(currentOffset)
      val (nextTodo, nextRemaining) = remaining.foldLeft((todo.tail, remaining)):
        case ((todo, remaining), candidate) =>
          findMatch(current, candidate) match
            case Some((scanner, offset)) =>
              (
                todo.updated(scanner, offset + currentOffset),
                remaining.filterNot(_ == candidate)
              )
            case None => (todo, remaining)
      search(nextBeacons, nextOffsets, nextTodo, nextRemaining)
  end search

  def result(scanners: Seq[Scanner]): (Set[Beacon], Seq[Beacon]) =
    search(Set(), Seq(), Map(scanners.head -> Beacon(0, 0, 0)), scanners.tail)

  def solve1(lines: String) = result(Parsing.parse(lines))._1.size
  def solve2(lines: String) = result(Parsing.parse(lines))._2
    .combinations(2)
    .map(pair => pair.head.manhattan(pair.last))
    .max

object Test:
  val file  = os.pwd / "2021" / "19" / "19.test.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 79
// Test.res2 // part 2: 3621

object Main:
  val file  = os.pwd / "2021" / "19" / "19.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 445
// Main.res2 // part 2: 13225
