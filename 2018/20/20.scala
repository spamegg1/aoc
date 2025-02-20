package aoc2018.day20

object DataDefs:
  enum Dir:
    case N, S, E, W
  import Dir.*

  extension (c: Char)
    def toDir = c match
      case 'N' => N
      case 'S' => S
      case 'E' => E
      case 'W' => W

  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def move(dir: Dir) = dir match
      case N => (p.x, p.y - 1)
      case S => (p.x, p.y + 1)
      case E => (p.x + 1, p.y)
      case W => (p.x - 1, p.y)

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def walk(
      remain: String,
      distances: Map[Pos, Int] = Map(),
      paths: Set[Pos] = Set(),
      stack: List[Set[Pos]] = Nil
  ): Seq[Int] = remain.head match
    case '^' => walk(remain.tail, Map((0, 0) -> 0), Set((0, 0)), Nil)
    case '$' => distances.values.toSeq
    case '(' => walk(remain.tail, distances, paths, Set() :: paths :: stack)
    case '|' =>
      val acc :: prev :: tail = stack: @unchecked
      walk(remain.tail, distances, prev, (acc ++ paths) :: prev :: tail)
    case ')' =>
      val acc :: prev :: tail = stack: @unchecked
      walk(remain.tail, distances, acc ++ paths, tail)
    case c =>
      val dir = c.toDir
      val (nextDistances, nextPaths) = paths.foldLeft((distances, Set.empty[Pos])) {
        case ((dists, paths), pos) =>
          val next = pos.move(dir)
          if dists.contains(next) && dists(next) <= dists(pos) + 1 then
            (dists, paths + next)
          else (dists.updated(next, dists(pos) + 1), paths + next)
      }
      walk(remain.tail, nextDistances, nextPaths, stack)

  def solve1(line: String)                 = walk(line).max
  def solve2(line: String)(threshold: Int) = walk(line).count(_ >= threshold)

object Test:
  lazy val file  = os.pwd / "2018" / "20" / "20.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1

object Main:
  lazy val file = os.pwd / "2018" / "20" / "20.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)(1000)

@main
def run: Unit =
  println(Test.res1) // part 1: 3,10,18,23,31
  println(Main.res1) // part 1: 3512
  println(Main.res2) // part 2: 8660
