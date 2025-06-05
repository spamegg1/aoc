package aoc2018.day22

object DataDefs:
  val MOD = 20183
  val X   = 16807
  val Y   = 48271

  enum Tool:
    case Neither, Torch, ClimbingGear
  import Tool.*

  enum Region(val risk: Int, val tools: Seq[Tool]):
    case Rocky  extends Region(0, Seq(Torch, ClimbingGear))
    case Wet    extends Region(1, Seq(Neither, ClimbingGear))
    case Narrow extends Region(2, Seq(Neither, Torch))

  type Pos   = (x: Int, y: Int)
  type Cave  = Pos => Region
  type State = (pos: Pos, tool: Tool)

  extension (p: Pos)
    def dist(that: Pos) = (p.x - that.x).abs + (p.y - that.y).abs
    def neighbors: Seq[Pos] = Seq(
      (p.x + 1, p.y),
      (p.x - 1, p.y),
      (p.x, p.y + 1),
      (p.x, p.y - 1)
    )

  extension (s: State)
    def movePos(cave: Cave): Seq[(State, Int)] = s.pos.neighbors
      .filter(next => next.x >= 0 && next.y >= 0)
      .filter(cave(_).tools.contains(s.tool))
      .map(p => (p, s.tool) -> 1)

    def changeTool(cave: Cave): Seq[(State, Int)] = cave(s.pos).tools
      .filterNot(_ == s.tool)
      .map((s.pos, _) -> 7)

    def neighbors(cave: Cave): Map[State, Int] =
      val move   = s.movePos(cave)
      val change = s.changeTool(cave)
      (move ++ change).toMap

object Cave:
  import DataDefs.*, Region.*

  def populate(depth: Int, target: Pos): Cave =
    val regions = Map(0 -> Rocky, 1 -> Wet, 2 -> Narrow)
    val erosion = collection.mutable.Map[Pos, Int]()

    def erosionLevel(pos: Pos): Int =
      erosion.getOrElseUpdate(pos, (geologicalIndex(pos) + depth) % MOD)

    def geologicalIndex(pos: Pos): Int = pos match
      case (0, 0)               => 0
      case (x, 0)               => X * x
      case (0, y)               => Y * y
      case pos if pos == target => 0
      case (x, y)               => erosionLevel((x - 1, y)) * erosionLevel((x, y - 1))

    (pos: Pos) => regions(erosionLevel(pos) % 3)

object Solving:
  import DataDefs.*, Tool.*, util.boundary, boundary.break
  import collection.mutable.{PriorityQueue, Map => MMap}

  def aStar(target: Pos, cave: Cave): Int =
    val start = (pos = (0, 0), tool = Torch)
    val end   = (pos = target, tool = Torch)
    val cost  = MMap[State, Int](start -> 0)
    val todo  = PriorityQueue[(State, Int)](start -> 0)(using Ordering.by(-_._2))
    var res   = -1
    def heuristic(state: State) = state.pos.dist(end.pos)

    boundary:
      while todo.nonEmpty do
        val (current, _) = todo.dequeue()
        if current == end then
          res = cost(end)
          break()
        else
          current
            .neighbors(cave)
            .foreach: (next, time) =>
              val nextCost = cost(current) + time
              if !cost.contains(next) || nextCost < cost(next) then
                cost(next) = nextCost
                val priority = nextCost + heuristic(next)
                todo.enqueue(next -> priority)
      end while
    res
  end aStar

  def solve1(target: Pos, depth: Int) =
    val cave = Cave.populate(depth, target)
    val risks =
      for
        x <- 0 to target.x
        y <- 0 to target.y
      yield cave((x, y)).risk
    risks.sum

  def solve2(target: Pos, depth: Int) = aStar(target, Cave.populate(depth, target))

object Test:
  lazy val target = (10, 10)
  lazy val depth  = 510
  lazy val res1   = Solving.solve1(target, depth)
  lazy val res2   = Solving.solve2(target, depth)

object Main:
  lazy val target = (9, 751)
  lazy val depth  = 11817
  lazy val res1   = Solving.solve1(target, depth)
  lazy val res2   = Solving.solve2(target, depth)

@main
def run: Unit =
  println(Test.res1) // part 1: 114
  println(Test.res2) // part 2: 45
  println(Main.res1) // part 1: 7402
  println(Main.res2) // part 2: 1025
