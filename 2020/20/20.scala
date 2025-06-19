package aoc2020.day20

import AdventOfCode2020.Day20.corners

object DataDefs:
  val seaMonster = Seq(
    (0, 1),
    (1, 2),
    (4, 2),
    (5, 1),
    (6, 1),
    (7, 2),
    (10, 2),
    (11, 1),
    (12, 1),
    (13, 2),
    (16, 2),
    (17, 1),
    (18, 0),
    (18, 1),
    (19, 1)
  )

  type Pos    = (x: Int, y: Int)
  type Pixels = Map[Pos, Char]
  type Grid   = Map[Pos, Tile]

  extension (p: Pos) def updated(dx: Int, dy: Int): Pos = (p.x + dx, p.y + dy)

  extension (p: Pixels)(using size: Int)
    def clock: Pixels = p.map((pos, pix) => (size - pos.y - 1, pos.x) -> pix)
    def horiz: Pixels = p.map((pos, pix) => (size - pos.x - 1, pos.y) -> pix)
    def vert: Pixels  = p.map((pos, pix) => (pos.x, size - pos.y - 1) -> pix)
    def all: Seq[Pixels] =
      for
        clock <- 0 to 3
        horiz <- 0 to 1
        vert  <- 0 to 1
      yield
        val rotated: Pixels = Iterator
          .iterate(p)(_.clock)
          .drop(clock)
          .next()
        val mirrored: Pixels = Iterator
          .iterate(rotated)(_.horiz)
          .drop(horiz)
          .next()
        Iterator
          .iterate(mirrored)(_.vert)
          .drop(vert)
          .next()
    def perms = p.all.distinct

  case class Image(size: Int, pixels: Pixels):
    val top    = (0 until size).map(x => pixels((x, 0)))
    val left   = (0 until size).map(y => pixels((0, y)))
    val bottom = (0 until size).map(x => pixels((x, size - 1)))
    val right  = (0 until size).map(y => pixels((size - 1, y)))
    def perms  = pixels.perms(using size).map(p => Image(size, p))

  case class Tile(id: Long, image: Image):
    export image.{top, left, bottom, right}
    val forward          = Set(top, left, bottom, right)
    val backward         = forward.map(_.reverse)
    val edges            = forward ++ backward
    def nbor(that: Tile) = that.edges.intersect(edges).size > 0
    def perms: Seq[Tile] = image.perms.map(Tile(id, _))

  case class Tiles(tiles: Seq[Tile]):
    // All tile edges match with only one other tile.
    // Therefore corner tiles will have 2 values with no matches
    // and edge tiles will have 1 value with no matches.
    // Those values will occur only once whereas values that match
    // with another tile will occur twice.
    val freqs = tiles
      .map(_.edges)
      .flatten
      .groupMapReduce(identity)(_ => 1)(_ + _)

    val categories = tiles.groupBy: tile =>
      tile.edges.count(edge => freqs(edge) == 1)

    // Corners are grouped under key 4 (two edges unique),
    // edges under key 2 and other tiles under key 0
    val corners = categories(4)

    val unscrambleTiles: Grid =
      val size   = math.sqrt(tiles.size).toInt
      val points = for x <- 0 until size; y <- 0 until size yield (x, y)

      // Arbitrarily choose the first corner
      val corner = corners.head.perms

      def helper(remain: Seq[Tile], points: Seq[Pos], ordered: Grid): Grid =
        if points.isEmpty then ordered
        else
          val tile = points.head match
            case (0, 0) =>
              corner
                .filter(tile => freqs(tile.top) == 1 && freqs(tile.left) == 1)
                .head
            case (0, y) =>
              remain
                .filter(tile => tile.top == ordered((0, y - 1)).bottom)
                .head
            case (x, y) =>
              remain
                .filter(tile => tile.left == ordered((x - 1, y)).right)
                .head
          helper(
            remain.filterNot(_.id == tile.id),
            points.tail,
            ordered.updated(points.head, tile)
          )
      end helper
      helper(tiles.flatMap(_.perms), points, Map())
    end unscrambleTiles

  extension (g: Grid)
    def assembleImage: Image = // assume g is unscrambled
      val size = 8 * math.sqrt(g.size).toInt
      val pixels: Pixels =
        (for
          x <- 0 until size
          y <- 0 until size
        yield (x, y) ->
          g((x / 8, y / 8)).image
            .pixels((1 + x % 8, 1 + y % 8))).toMap
      Image(size, pixels)

object Parsing:
  import DataDefs.*

  def parse(lines: String): Tiles = Tiles:
    lines.trim
      .split("\n\n")
      .toSeq
      .map(_.split("\n").map(_.trim))
      .map: tile =>
        val id   = tile.head.slice(5, 9).toLong
        val data = tile.drop(1)
        val pixels: Pixels =
          (for
            y <- 0 to 9
            x <- 0 to 9
          yield (x, y) -> data(y)(x)).toMap
        Tile(id, Image(10, pixels))

object Solving:
  import DataDefs.*

  def findSeaMonsters(image: Image): Seq[Int] =
    for candidate <- image.perms
    yield
      val matches: Seq[Pos] =
        for
          x <- 0 until image.size - 20 // Sea monster dimensions are 20 x 3
          y <- 0 until image.size - 3
          if seaMonster
            .map((x, y).updated)
            .forall(pos => candidate.pixels(pos) == '#')
        yield (x, y)
      val monsters = matches
        .flatMap: pos =>
          seaMonster.map(pos.updated)
        .toSet
      candidate.pixels.keys
        .count: pos =>
          candidate.pixels(pos) == '#' &&
            !monsters.contains(pos)

  def solve1(lines: String) = Parsing
    .parse(lines)
    .corners
    .map(_.id)
    .product

  def solve2(lines: String) =
    val completeImage = Parsing
      .parse(lines)
      .unscrambleTiles
      .assembleImage
    findSeaMonsters(completeImage).min

object Test:
  lazy val file  = os.pwd / "2020" / "20" / "20.test.input.txt"
  lazy val lines = os.read(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "20" / "20.input.txt"
  lazy val lines = os.read(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 20899048083289
  println(Test.res2) // part 2: 273
  println(Main.res1) // part 1: 60145080587029
  println(Main.res2) // part 2: 1901
