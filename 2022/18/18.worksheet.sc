object DataDefs:
  val Ortho = Seq((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))
  case class Cube(x: Int, y: Int, z: Int):
    def delta(dx: Int, dy: Int, dz: Int): Cube = Cube(x + dx, y + dy, z + dz)
    def neighbours: Seq[Cube]                  = Ortho.map(delta)

object Parsing:
  import DataDefs.*

  def parse(input: Seq[String]): Set[Cube] = input.toSet.map: line =>
    val Array(x, y, z) = line.split(",").map(_.toInt)
    Cube(x, y, z)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]): Int =
    val cubes = Parsing.parse(lines)
    cubes.toSeq.map(_.neighbours.filterNot(cubes.contains).size).sum

  def solve2(input: Seq[String]): Int =
    val cubes   = Parsing.parse(input)
    val xs      = cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1
    val ys      = cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1
    val zs      = cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1
    val start   = Cube(xs.head, ys.head, zs.head)
    val todo    = collection.mutable.Queue(start)
    val visited = collection.mutable.Set(start)

    while todo.nonEmpty do
      todo
        .dequeue()
        .neighbours
        .filterNot(cubes.contains)
        .filterNot(visited.contains)
        .foreach: next =>
          if xs.contains(next.x) && ys.contains(next.y) && zs.contains(next.z) then
            todo.enqueue(next)
            visited += next
    end while
    cubes.toSeq.map(_.neighbours.count(visited.contains)).sum
end Solving

object Test:
  val file  = os.pwd / "2022" / "18" / "18.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 64
// Test.res2 // part 2: 58

object Main:
  val file  = os.pwd / "2022" / "18" / "18.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 3662
// Main.res2 // part 2: 2060
