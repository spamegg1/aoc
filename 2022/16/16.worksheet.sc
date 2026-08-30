object DataDefs:
  type Label = String
  case class Valve(flow: Int, neighbours: Seq[Label])
  type Valves    = Map[Label, Valve]
  type Distances = Map[Label, Int]
  type Graph     = Map[Label, Distances]
  type Todo      = Set[Label]
end DataDefs

object BFS:
  import DataDefs.*

  def bfs(valves: Valves, start: Label): Distances =
    val todo = collection.mutable.Queue(start)
    val cost = collection.mutable.Map(start -> 1)
    while todo.nonEmpty do
      val current = todo.dequeue()
      valves(current).neighbours
        .filterNot(cost.contains)
        .foreach: next =>
          todo.enqueue(next)
          cost(next) = cost(current) + 1
    cost.toMap
end BFS

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): (Valves, Graph, Todo) =
    val valves = lines
      .map: line =>
        val Array(_, name, flow, edges*) = line.split("[^A-Z0-9]+"): @unchecked
        name -> Valve(flow.toInt, edges)
      .toMap
    val graph = valves.map((k, v) => k -> BFS.bfs(valves, k))
    val todo  = valves.filter((k, v) => v.flow > 0).keySet
    (valves, graph, todo)
end Parsing

object Solving:
  import DataDefs.*

  def explore(valves: Valves, graph: Graph, todo: Todo, init: Int): Map[Todo, Int] =
    val score = collection.mutable.Map[Set[String], Int]().withDefaultValue(0)
    def step(todo: Todo, done: Todo, from: Label, time: Int, pressure: Int): Unit =
      score(done) = score(done).max(pressure)
      for
        next <- todo
        remaining = time - graph(from)(next)
        if remaining > 0
        extra = remaining * valves(next).flow
      do step(todo - next, done + next, next, remaining, pressure + extra)
    end step
    step(todo, Set(), "AA", init, 0)
    score.toMap
  end explore

  def solve1(lines: Seq[String]) =
    val (valves, graph, todo) = Parsing.parse(lines)
    explore(valves, graph, todo, 30).values.max

  def solve2(lines: Seq[String]) =
    val (valves, graph, todo) = Parsing.parse(lines)
    val sets                  = explore(valves, graph, todo, 26)
    val disjoint =
      for
        (you, score1)      <- sets
        (elephant, score2) <- sets
        if you.intersect(elephant).isEmpty
      yield score1 + score2
    disjoint.max

object Test:
  val file  = os.pwd / "2022" / "16" / "16.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 1651
// Test.res2 // part 2: 1707

object Main:
  val file  = os.pwd / "2022" / "16" / "16.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1741
// Main.res2 // part 2: 2316
