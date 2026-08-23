object DataDefs:
  type Pos    = (x: Int, y: Int)
  type Algo   = Seq[Int]
  type Pixels = Map[Pos, Int]
  type Image  = (min: Int, max: Int, pixels: Pixels)

  extension (c: Char) def toDigit = if c == '#' then 1 else 0
  extension (a: Algo) def binary  = Integer.parseInt(a.mkString, 2)

  extension (p: Pos)
    def neighbours: Seq[Pos] =
      for
        dy <- -1 to 1
        dx <- -1 to 1
      yield (p.x + dx, p.y + dy)

  extension (i: Image)
    def indexAt(pos: Pos, default: Int): Int = pos.neighbours
      .map(i.pixels.getOrElse(_, default))
      .binary
end DataDefs

object Parsing:
  import DataDefs.*

  def parseAlgo(line: String): Algo = line.map(_.toDigit)

  def parseImg(lines: Seq[String]): Image =
    val max = lines.size
    val pixels =
      for
        y <- 0 until max
        x <- 0 until max
      yield (x, y) -> lines(y)(x).toDigit
    (-1, max, pixels.toMap)

object Solving:
  import DataDefs.*

  def step(algo: Algo)(default: Int, img: Image): (Int, Image) =
    val pixels =
      for
        y <- img.min to img.max
        x <- img.min to img.max
        index = img.indexAt((x, y), default)
      yield (x = x, y = y) -> algo(index)

    val nextDefault = if default == 0 then algo.head else algo.last
    val nextImg     = (img.min - 1, img.max + 1, pixels.toMap)
    (nextDefault, nextImg)
  end step

  def enhance(algo: Algo, img: Image, steps: Int): Int = Iterator
    .iterate((0, img))(step(algo))
    .drop(steps)
    .next()
    ._2
    .pixels
    .values
    .sum

  def solve(steps: Int)(lines: Seq[String], line: String) =
    enhance(Parsing.parseAlgo(line), Parsing.parseImg(lines), steps)

  val solve1 = solve(2)
  val solve2 = solve(50)

object Test:
  val file1 = os.pwd / "2021" / "20" / "20.test.input.1.txt"
  val file2 = os.pwd / "2021" / "20" / "20.test.input.2.txt"
  val lines = os.read.lines(file1)
  val line  = os.read(file2)
  val res1  = Solving.solve1(lines, line)
  val res2  = Solving.solve2(lines, line)
// Test.res1 // part 1: 35
// Test.res2 // part 2: 3351

object Main:
  val file1 = os.pwd / "2021" / "20" / "20.input.1.txt"
  val file2 = os.pwd / "2021" / "20" / "20.input.2.txt"
  val lines = os.read.lines(file1)
  val line  = os.read(file2)
  val res1  = Solving.solve1(lines, line)
  val res2  = Solving.solve2(lines, line)
// Main.res1 // part 1: 5573
// Main.res2 // part 2: 20097
