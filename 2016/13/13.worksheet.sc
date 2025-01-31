object DataDefs:
  type Pos  = (x: Long, y: Long)
  type Path = Seq[Pos]
  type Cap  = Path => Boolean

  extension (p: Pos)
    def inBounds            = 0 <= p.x && 0 <= p.y
    def check               = p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y
    def isOpen(input: Long) = (p.check + input).toBinaryString.count(_ == '1') % 2 == 0
    def neighbors(input: Long) = Seq(
      (x = p.x - 1, y = p.y),
      (x = p.x + 1, y = p.y),
      (x = p.x, y = p.y + 1),
      (x = p.x, y = p.y - 1)
    ).filter(_.inBounds).filter(_.isOpen(input))

object Solving:
  import DataDefs.*, util.boundary, boundary.break, collection.mutable.Queue

  def findPath(cap: Cap)(input: Long)(start: Pos, goal: Pos) =
    val startPath = Seq(start)                // path
    var visited   = Seq(start)                // set of visited positions
    val queue     = Queue((start, startPath)) // queue of pos, path to explore next
    var finalPath = startPath                 // final path

    boundary:
      while queue.nonEmpty do
        val (current, path) = queue.dequeue()
        finalPath = path
        if current == goal then break()

        val neighbors = if cap(path) then current.neighbors(input) else Seq()
        for neighbor <- neighbors do
          if !visited.contains(neighbor) then
            visited = neighbor +: visited
            queue.enqueue((neighbor, path :+ current))

    (finalPath, visited)

  val solve1 = findPath(_ => true)
  val solve2 = findPath(_.size <= 50)

object Test:
  lazy val input = 10L
  lazy val start = (x = 1L, y = 1L)
  lazy val goal  = (x = 7L, y = 4L)
  lazy val res   = Solving.solve1(input)(start, goal)._1.size - 1
// Test.res // part 1: 11

object Main:
  lazy val input = 1358L
  lazy val start = (x = 1L, y = 1L)
  lazy val goal  = (x = 31L, y = 39L)
  lazy val res1  = Solving.solve1(input)(start, goal)._1.size - 1
  lazy val res2  = Solving.solve2(input)(start, goal)._2.size
// Main.res1 // part 1: 96
// Main.res2 // part 2: 141
