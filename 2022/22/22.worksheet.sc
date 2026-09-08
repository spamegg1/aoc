object DataDefs:
  case class Point(x: Int, y: Int):
    def +(other: Point)  = Point(x + other.x, y + other.y)
    def clockwise: Point = Point(-y, x)
    def counterClockwise = Point(y, -x)
    def score: Int       = 1000 * (y + 1) + 4 * (x + 1)

  case class Vec(x: Int, y: Int, z: Int):
    def *(k: Int)     = Vec(x * k, y * k, z * k)
    def +(b: Vec)     = Vec(x + b.x, y + b.y, z + b.z)
    def cross(b: Vec) = Vec(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x)

  case class Info(point: Point, i: Vec, j: Vec, k: Vec)
end DataDefs

object Parsing:
  import DataDefs.*

  def parseTiles(lines: Seq[String]): Map[Point, Boolean] =
    val points =
      for
        (row, y)  <- lines.zipWithIndex
        (cell, x) <- row.zipWithIndex
        if cell != ' '
      yield Point(x, y) -> (cell == '.')
    points.toMap

  def parseMoves(line: String) = line
    .replace("L", " L ")
    .replace("R", " R ")
    .split(" ")
    .toSeq
    .flatMap(token => if token.head.isDigit then "F" * token.toInt else token)

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(path: String) =
    val tiles = Parsing.parseTiles(lines)
    val moves = Parsing.parseMoves(path)
    val minX  = tiles.keys.groupMapReduce(_.y)(_.x)(_ min _)
    val maxX  = tiles.keys.groupMapReduce(_.y)(_.x)(_ max _)
    val minY  = tiles.keys.groupMapReduce(_.x)(_.y)(_ min _)
    val maxY  = tiles.keys.groupMapReduce(_.x)(_.y)(_ max _)

    val (right, down, left, up) = (Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))
    val facing                  = Seq(right, down, left, up)
    val topLeft                 = tiles.keys.filter(_.y == 0).minBy(_.x)

    val (position, direction) = moves.foldLeft((topLeft, right)):
      case ((position, direction), move) =>
        move match
          case 'L' => (position, direction.counterClockwise)
          case 'R' => (position, direction.clockwise)
          case _ =>
            val next = position + direction
            tiles.get(next) match
              case Some(true)  => (next, direction)
              case Some(false) => (position, direction)
              case None =>
                val wrapPosition = direction match
                  case `right` => position.copy(x = minX(position.y))
                  case `left`  => position.copy(x = maxX(position.y))
                  case `down`  => position.copy(y = minY(position.x))
                  case `up`    => position.copy(y = maxY(position.x))
                  case _       => position
                if tiles(wrapPosition) then (wrapPosition, direction)
                else (position, direction)
    position.score + facing.indexOf(direction)
  end solve1

  def solve2(lines: Seq[String])(path: String)(block: Int) =
    val tiles    = Parsing.parseTiles(lines)
    val moves    = Parsing.parseMoves(path)
    val scaleIJ  = block - 1
    val scaleK   = block + 1
    val startPos = Vec(-scaleIJ, -scaleIJ, -scaleK)
    val startDir = Vec(2, 0, 0)
    val topLeft  = tiles.keys.filter(_.y == 0).minBy(_.x)
    val start    = Info(topLeft, Vec(1, 0, 0), Vec(0, 1, 0), Vec(0, 0, 1))
    val todo     = collection.mutable.Queue(start)
    val visited  = collection.mutable.Set(topLeft)
    val points   = collection.mutable.Map[Vec, Info]()

    while todo.nonEmpty do
      val Info(offset, i, j, k) = todo.dequeue()
      for x <- 0 until block do
        for y <- 0 until block do
          // Scale by 2 to keep points integer
          val key = (i * (2 * x - scaleIJ)) + (j * (2 * y - scaleIJ)) + (k * -scaleK)
          points(key) = Info(offset + Point(x, y), i, j, k)
      val neighbours = Seq(
        Info(offset + Point(-block, 0), j.cross(i), j, j.cross(k)), // Left
        Info(offset + Point(block, 0), i.cross(j), j, k.cross(j)),  // Right
        Info(offset + Point(0, -block), i, j.cross(i), k.cross(i)), // Up
        Info(offset + Point(0, block), i, i.cross(j), i.cross(k))   // Down
      )
      neighbours.foreach: next =>
        if tiles.contains(next.point) && !visited.contains(next.point) then
          todo += next
          visited += next.point
    end while

    val (position, direction) = moves.foldLeft((startPos, startDir)):
      case ((pos, dir), move) =>
        move match
          case 'L' => (pos, dir.cross(points(pos).k))
          case 'R' => (pos, points(pos).k.cross(dir))
          case _ =>
            val next = pos + dir
            if points.contains(next) then
              if tiles(points(next).point) then (next, dir)
              else (pos, dir)
            else
              val wrapDirection = points(pos).k * 2 // This is the fun part
              val wrapPosition  = next + wrapDirection
              if tiles(points(wrapPosition).point) then (wrapPosition, wrapDirection)
              else (pos, dir)
    val Info(point, i, j, k) = points(position)
    point.score + Seq(i * 2, j * 2, i * -2, j * -2).indexOf(direction)

object Test:
  val file  = os.pwd / "2022" / "22" / "22.test.input.txt"
  val lines = os.read.lines(file)
  val path  = "10R5L5R10L4R5L5"
  val res1  = Solving.solve1(lines)(path)
  val res2  = Solving.solve2(lines)(path)(4)
// Test.res1 // part 1: 6032
// Test.res2 // part 2: 5031

object Main:
  val file1 = os.pwd / "2022" / "22" / "22.input.1.txt"
  val file2 = os.pwd / "2022" / "22" / "22.input.2.txt"
  val lines = os.read.lines(file1)
  val path  = os.read.lines(file2).head
  val res1  = Solving.solve1(lines)(path)
  val res2  = Solving.solve2(lines)(path)(50)
// Main.res1 // part 1: 31568
// Main.res2 // part 2: 36540
