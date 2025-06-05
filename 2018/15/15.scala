package aoc2018.day15

object DataDefs:
  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def neighbors = Seq(
      (p.x, p.y - 1),
      (p.x - 1, p.y),
      (p.x + 1, p.y),
      (p.x, p.y + 1)
    )

  type Kind     = Char
  type HP       = Int
  type Dmg      = Int
  type Turn     = Int
  type Score    = Int
  type Count    = Int
  type Creature = (pos: Pos, kind: Kind, health: HP, dmg: Dmg, turn: Turn)
  type World    = (walls: Set[Pos], creatures: Set[Creature])

object Creature:
  import DataDefs.*
  extension (c: Creature)
    def passTurn            = (c.pos, c.kind, c.health, c.dmg, c.turn + 1)
    def changePos(pos: Pos) = (pos, c.kind, c.health, c.dmg, c.turn + 1)
    def changeHp(newHp: HP) = (c.pos, c.kind, newHp, c.dmg, c.turn)

    def move(enemies: Map[Pos, Creature], obstacles: Set[Pos]): Creature =
      if c.pos.neighbors.exists(enemies.contains) then c.passTurn
      else
        val candidates: Set[Pos] = enemies.keySet.flatMap(_.neighbors)
        BFS.bfs(c.pos, candidates, obstacles) match
          case Some(pos) => c.changePos(pos)
          case None      => c.passTurn

    def attack(enemies: Map[Pos, Creature], creatures: Set[Creature]): Set[Creature] =
      c.pos.neighbors
        .flatMap(enemies.get)
        .minOption(using Orders.attackOrder) match
        case Some(enemy) =>
          val nextHealth = enemy.health - c.dmg
          if nextHealth <= 0 then creatures - enemy
          else creatures - enemy + enemy.changeHp(nextHealth)
        case None => creatures

object World:
  import DataDefs.*, Creature.*

  extension (w: World)
    def fight: (Kind, Count, Score) =
      if w.creatures.map(_.kind).size == 1 then
        val winner    = w.creatures.head.kind
        val survivors = w.creatures.size
        val score     = w.creatures.map(_.turn).min * w.creatures.toSeq.map(_.health).sum
        (winner, survivors, score)
      else
        val current: Creature   = w.creatures.min(using Orders.turnOrder)
        val obstacles: Set[Pos] = w.walls ++ w.creatures.map(_.pos)
        val enemies: Map[Pos, Creature] = w.creatures
          .filter(_.kind != current.kind)
          .map(creature => creature.pos -> creature)
          .toMap
        val moved: Creature         = current.move(enemies, obstacles)
        val attacked: Set[Creature] = moved.attack(enemies, w.creatures)
        val newWorld                = (w.walls, attacked - current + moved)
        newWorld.fight

object BFS:
  import DataDefs.*
  def bfs(start: Pos, candidates: Set[Pos], obstacles: Set[Pos]): Option[Pos] =
    @annotation.tailrec
    def helper(
        todo: Seq[Pos],
        paths: Map[Pos, Seq[Pos]],
        found: Option[Seq[Pos]]
    ): Option[Pos] =
      if todo.isEmpty then found.map(_.head)
      else
        val pos  = todo.head
        val cost = paths(pos).size

        val (nextTodo, nextPaths) = pos.neighbors
          .filterNot(obstacles.contains)
          .filter(next => found.forall(_.size > cost + 1))
          .filter(next => paths.get(next).forall(_.size > cost + 1))
          .foldLeft((todo.tail, paths)) { case ((todo, paths), next) =>
            (todo.appended(next), paths.updated(next, paths(pos).appended(next)))
          }

        val nextFound =
          if !candidates.contains(pos) then found
          else
            found match
              case None => Some(paths(pos))
              case Some(other) =>
                if cost < other.size then Some(paths(pos)) else Some(other)

        helper(nextTodo, nextPaths, nextFound)
    helper(Seq(start), Map(start -> Seq()), None)

object Orders:
  import DataDefs.*
  given readingOrder: Ordering[Pos]   = Ordering.by(pos => (pos.y, pos.x))  // needed
  val turnOrder: Ordering[Creature]   = Ordering.by(c => (c.turn, c.pos))   // needs above
  val attackOrder: Ordering[Creature] = Ordering.by(c => (c.health, c.pos)) // needs above

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String], elfAttackPower: Int): (Set[Pos], Set[Creature]) =
    val poses =
      for
        y <- lines.indices
        x <- lines.head.indices
      yield (x = x, y = y) -> lines(y)(x)

    val walls = poses
      .collect:
        case (p, '#') => p
      .toSet

    val elves = poses
      .collect:
        case (pos, 'E') => (pos, 'E', 200, elfAttackPower, 0)
      .toSet

    val goblins = poses
      .collect:
        case (pos, 'G') => (pos, 'G', 200, 3, 0)
      .toSet

    (walls, elves ++ goblins)

object Solving:
  import DataDefs.*, World.*

  def solve1(lines: Seq[String]) =
    val world         = Parsing.parse(lines, 3)
    val (_, _, score) = world.fight
    score

  def solve2(lines: Seq[String]) =
    @annotation.tailrec
    def helper(elfAttackPower: Int): Int =
      val world: World               = Parsing.parse(lines, elfAttackPower)
      val (winner, survivors, score) = world.fight
      if winner == 'E' && survivors == world.creatures.count(_.kind == 'E') then score
      else helper(elfAttackPower + 1)
    helper(3)

object Test:
  lazy val file1 = os.pwd / "2018" / "15" / "15.test.input.1.txt"
  lazy val file2 = os.pwd / "2018" / "15" / "15.test.input.2.txt"
  lazy val file3 = os.pwd / "2018" / "15" / "15.test.input.3.txt"
  lazy val file4 = os.pwd / "2018" / "15" / "15.test.input.4.txt"
  lazy val file5 = os.pwd / "2018" / "15" / "15.test.input.5.txt"
  lazy val file6 = os.pwd / "2018" / "15" / "15.test.input.6.txt"
  lazy val files = Seq(file1, file2, file3, file4, file5, file6)
  lazy val lines = files map os.read.lines
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2

object Main:
  lazy val file  = os.pwd / "2018" / "15" / "15.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 27730,36334,39514,27755,28944,18740
  println(Test.res2) // part 2: 4988,29064,31284,3478,6474,1140
  println(Main.res1) // part 1: 197025
  println(Main.res2) // part 2: 44423
