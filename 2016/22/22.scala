package aoc2016.day22

object DataDefs:
  enum State:
    case Full, Empty, Moveable
    def toChar = this match
      case Full     => '#'
      case Empty    => '_'
      case Moveable => '.'
  import State.*

  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def neighbors = Seq(
      (x = p.x - 1, y = p.y),
      (x = p.x + 1, y = p.y),
      (x = p.x, y = p.y - 1),
      (x = p.x, y = p.y + 1)
    )

  case class Node(pos: Pos, size: Int, used: Int, avail: Int, use: Int):
    def state(emptySize: Int) =
      if used > emptySize then Full
      else if used == 0 then Empty
      else Moveable
    def show(start: Pos, goal: Pos, emptySize: Int) =
      if pos == start then '@' else if pos == goal then 'G' else state(emptySize).toChar

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Node = line match
    case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T ${use}%" =>
      Node((x = x.toInt, y = y.toInt), size.toInt, used.toInt, avail.toInt, use.toInt)

  def parse(lines: Seq[String]): Seq[Node] = lines map parseLine
  def graph(nodes: Seq[Node])(row: Int, start: Pos, goal: Pos, emptySize: Int): String =
    nodes
      .map(_.show(start, goal, emptySize))
      .grouped(row)
      .toSeq
      .map(_.mkString)
      .mkString("\n")

object Solving:
  import DataDefs.*, State.*, util.boundary, boundary.break, collection.mutable.Queue

  // checks if we can move a into b
  def viable(a: Node, b: Node): Boolean = a.used != 0 && a != b && a.used <= b.avail

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .combinations(2)
    .count { case Seq(a, b) => viable(a, b) || viable(b, a) }

  def solve2(lines: Seq[String]) =
    val nodes = Parsing.parse(lines)
    val empty = nodes.find(_.used == 0).get               // first find the empty node
    val goal  = nodes.filter(_.pos.y == 0).maxBy(_.pos.x) // find node with y=0 and max x
    val queue = Queue(empty.pos -> 0)                     // position, number of steps
    var visit = Set(empty.pos)                            // visited positions
    var res   = 0                                         // number of steps
    val grid  = nodes.map(node => node.pos -> node.state(empty.size)).toMap

    // val target = os.pwd / "2016" / "22" / "grid.txt"
    // val graph  = Parsing.graph(nodes)(29, empty.pos, goal.pos, empty.size)
    // os.write(target, graph) // this treats x=row (0-34), y=col (0-28)

    // We can use this information: to get the goal data to the top left,
    // we have to first move the empty spot to directly above the goal data.
    // After we’ve done that, we ‘walk’ it up by moving the empty data
    // down, right, up, up, left in a continuous loop.
    // println(s"empty: ${empty.pos}") // 8,28, move this to 33,0
    // println(s"goal: ${goal.pos}")   // 34,0, it takes (34-1)*5 moves to reach 0,0
    boundary:
      while queue.nonEmpty do
        val (pos, steps) = queue.dequeue()
        if pos == goal.pos then
          res = steps + (goal.pos.x - 1) * 5
          break()
        for neighbor <- pos.neighbors do
          grid.get(neighbor) match
            case None => () // neighbor is outside grid, ignore
            case Some(value) =>
              if value == Moveable && !visit.contains(neighbor) then
                visit += neighbor
                queue.enqueue((neighbor, steps + 1))
    res

object Test:
  lazy val file  = os.pwd / "2016" / "22" / "22.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2016" / "22" / "22.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 7
  println(Test.res2) // part 2: 7
  println(Main.res1) // part 1: 981
  println(Main.res2) // part 2: 233
