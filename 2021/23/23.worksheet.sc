object DataDefs:
  val Costs = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val Rooms = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)

  type Room  = Seq[Char]
  type Rooms = Map[Char, Seq[Char]]

  case class Burrow(roomMax: Int, cost: Int, hallway: Room, rooms: Rooms):
    def finished = rooms.forall: (kind, room) =>
      room.size == roomMax && room.forall(_ == kind)

    def hallwayToRoom(amphipod: Char, start: Int): Option[Burrow] =
      val end   = Rooms(amphipod)
      val range = if start < end then ((start + 1) to end) else (end to (start - 1))

      if rooms(amphipod).forall(_ == amphipod) && range.forall(n => hallway(n) == '.')
      then
        val nextH = hallway.updated(start, '.')
        val nextR = rooms.updated(amphipod, rooms(amphipod).prepended(amphipod))
        val nextC = cost + (range.size + roomMax - rooms(amphipod).size) * Costs(amphipod)
        Some(Burrow(roomMax, nextC, nextH, nextR))
      else None
    end hallwayToRoom

    def roomToRoom(key: Char, room: Seq[Char]): Seq[Burrow] =
      if room.forall(_ == key) then Seq()
      else
        val start = Rooms(key)
        val end   = Rooms(room.head)
        val range = if start < end then start to end else end to start
        if rooms(room.head).forall(_ == room.head) &&
          range.forall(n => hallway(n) == '.')
        then
          val nextR = rooms
            .updated(key, room.tail)
            .updated(room.head, rooms(room.head).prepended(room.head))
          val c = range.size + roomMax - rooms(key).size + roomMax - rooms(room.head).size
          val nextC = cost + c * Costs(room.head)
          Seq(Burrow(roomMax, nextC, hallway, nextR))
        else Seq()
    end roomToRoom

    def roomToHallway(key: Char, room: Seq[Char]): Seq[Burrow] =
      if room.forall(_ == key) then return Seq()
      val index = Rooms(key)
      val valid = Seq(0, 1, 3, 5, 7, 9, 10)
      val left  = valid.filter(_ < index).reverse.takeWhile(n => hallway(n) == '.')
      val right = valid.filter(_ > index).takeWhile(n => hallway(n) == '.')
      (left.reverse ++ right).map: pos =>
        val nextH = hallway.updated(pos, room.head)
        val nextR = rooms.updated(key, room.tail)
        val cst   = (pos - index).abs + 1 + roomMax - room.size
        val nextC = cost + cst * Costs(room.head)
        Burrow(roomMax, nextC, nextH, nextR)
    end roomToHallway

    def move(energy: Option[Int]): Option[Int] =
      if finished then Some(cost)
      else if energy.exists(_ < cost) then None
      else
        paths
          .map(_.move(energy))
          .flatten
          .minOption

    def shuffle: Option[Int] = move(None)

    def paths: Seq[Burrow] =
      val first = hallway.zipWithIndex
        .filter(_._1 != '.')
        .flatMap(hallwayToRoom)
      val second    = rooms.flatMap(roomToRoom)
      val preferred = first ++ second
      if preferred.nonEmpty then preferred
      else rooms.flatMap(roomToHallway).toSeq
  end Burrow
end DataDefs

object Parsing:
  import DataDefs.*

  def roomSmall(lines: Seq[String])(column: Int) =
    Seq(lines(2)(column), lines(3)(column))

  def roomBig(lines: Seq[String])(column: Int, second: Char, third: Char) =
    Seq(lines(2)(column), second, third, lines(3)(column))

  def parseSmall(lines: Seq[String]) =
    val a = roomSmall(lines)(3)
    val b = roomSmall(lines)(5)
    val c = roomSmall(lines)(7)
    val d = roomSmall(lines)(9)
    Burrow(2, 0, Seq.fill(11)('.'), Map('A' -> a, 'B' -> b, 'C' -> c, 'D' -> d))

  def parseBig(lines: Seq[String]) =
    val a = roomBig(lines)(3, 'D', 'D')
    val b = roomBig(lines)(5, 'C', 'B')
    val c = roomBig(lines)(7, 'B', 'A')
    val d = roomBig(lines)(9, 'A', 'C')
    Burrow(4, 0, Seq.fill(11)('.'), Map('A' -> a, 'B' -> b, 'C' -> c, 'D' -> d))
end Parsing

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parseSmall(lines)
    .shuffle
    .get

  def solve2(lines: Seq[String]) = Parsing
    .parseBig(lines)
    .shuffle
    .get
end Solving

object Test:
  val file  = os.pwd / "2021" / "23" / "23.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 12521
// Test.res2 // part 2: 44169

object Main:
  val file  = os.pwd / "2021" / "23" / "23.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 18170
// Main.res2 // part 2: 50208
