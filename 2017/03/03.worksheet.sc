object DataDefs:
  case class Spiral(number: Int):
    lazy val segment = math.ceil((math.sqrt(number) - 1) / 2.0).toInt
    lazy val offset  = number - math.pow(2 * segment - 1, 2).toInt
    lazy val position =
      if 1 <= offset && offset <= 2 * segment then (segment, -segment + offset)
      else if 2 * segment <= offset && offset <= 4 * segment then
        (3 * segment - offset, segment)
      else if 4 * segment <= offset && offset <= 6 * segment then
        (-segment, 5 * segment - offset)
      else (-7 * segment + offset, -segment)
    lazy val distance = math.abs(position._1) + math.abs(position._2)

  case class Pos(x: Int, y: Int):
    lazy val next: Pos =
      if y < x then // bottom edge or right edge, excluding top-right bottom-left corners
        if math.abs(x) <= math.abs(y) then Pos(x + 1, y) // bottom edge incl. bot-rgh
        else Pos(x, y + 1)                               // right edge
      else                                               // top edge or left edge
      if math.abs(x) < math.abs(y) then Pos(x - 1, y)    // top edge excl. top-left c.
      else if x == y then
        if 0 < x then Pos(x - 1, y) // top-right corner
        else Pos(x + 1, y)          // bottom-left corner
      else Pos(x, y - 1)            // left edge incl. top-left corner

    lazy val neighbors = Seq(
      Pos(x - 1, y + 1),
      Pos(x, y + 1),
      Pos(x + 1, y + 1),
      Pos(x - 1, y),
      Pos(x + 1, y),
      Pos(x - 1, y - 1),
      Pos(x, y - 1),
      Pos(x + 1, y - 1)
    )

object Solving:
  import DataDefs.*

  def solve1(input: Int): Int = Spiral(input).distance

  def solve2(input: Int): Int =
    var value = 1
    var pos   = Pos(0, 0)
    val map   = collection.mutable.Map[Pos, Int](pos -> value)

    while value <= input do
      pos = pos.next
      value = pos.neighbors.map(neighbor => map.getOrElse(neighbor, 0)).sum
      map(pos) = value

    value

object Test:
  lazy val res1 = Solving.solve1(21)
  lazy val res2 = Solving.solve2(747)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 806

object Main:
  lazy val input = 361527
  lazy val res1  = Solving.solve1(input)
  lazy val res2  = Solving.solve2(input)
// Main.res1 // part 1: 326
// Main.res2 // part 2: 363010
