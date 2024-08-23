/*
--- Day 8: Space Image Format ---
The Elves' spirits are lifted when they realize you have an opportunity to
reboot one of their Mars rovers, and so they are curious if you would spend
a brief sojourn on Mars. You land your ship near the rover.

When you reach the rover, you discover that it's already in the process of rebooting!
It's just waiting for someone to enter a BIOS password.
The Elf responsible for the rover takes a picture of the password
(your puzzle input) and sends it to you via the Digital Sending Network.

Unfortunately, images sent via the Digital Sending Network aren't encoded with
any normal encoding; instead, they're encoded in a special Space Image Format.
None of the Elves seem to remember why this is the case.
They send you the instructions to decode it.

Images are sent as a series of digits that each represent the color of a single pixel.
The digits fill each row of the image left-to-right, then move downward to the next row,
filling rows top-to-bottom until every pixel of the image is filled.

Each image actually consists of a series of identically-sized
layers that are filled in this way. So, the first digit corresponds
to the top-left pixel of the first layer, the second digit corresponds
to the pixel to the right of that on the same layer, and so on until
the last digit, which corresponds to the bottom-right pixel of the last layer.

For example, given an image 3 pixels wide and 2 pixels tall,
the image data 123456789012 corresponds to the following image layers:

Layer 1: 123
         456

Layer 2: 789
         012

The image you received is 25 pixels wide and 6 pixels tall.

To make sure the image wasn't corrupted during transmission,
the Elves would like you to find the layer that contains the fewest 0 digits.
On that layer, what is the number of 1 digits multiplied by the number of 2 digits?

--- Part Two ---
Now you're ready to decode the image.
The image is rendered by stacking the layers and aligning the pixels
with the same positions in each layer.
The digits indicate the color of the corresponding pixel:
  0 is black, 1 is white, and 2 is transparent.

The layers are rendered with the first layer in front and the last layer in back.
So, if a given position has a transparent pixel in the first and second layers,
a black pixel in the third layer, and a white pixel in the fourth layer,
the final image would have a black pixel at that position.

For example, given an image 2 pixels wide and 2 pixels tall,
the image data 0222112222120000 corresponds to the following image layers:
Layer 1: 02
         22
Layer 2: 11
         22
Layer 3: 22
         12
Layer 4: 00
         00

Then, the full image can be found by determining the top visible pixel in each position:
  The top-left pixel is black because the top layer is 0.
  The top-right pixel is white because the top layer is 2 (transparent),
    but the second layer is 1.
  The bottom-left pixel is white because the top two layers are 2,
    but the third layer is 1.
  The bottom-right pixel is black because the only visible pixel in that position is 0
    (from layer 4).

So, the final image looks like this:
01
10

What message is produced after decoding your image?
 */
object DataDefs:
  case class Dim(w: Int, h: Int):
    lazy val size = w * h

  extension (s: String)
    def seeThru: Char = s.foldLeft('2'): (result, char) =>
      if result == '2' then char else result

  case class Layer(rows: Seq[String])(using d: Dim):
    lazy val size = rows.size
    def count(num: Char): Int = rows.mkString.count(_ == num)
    lazy val part1 = count('1') * count('2')
    lazy val show = rows.mkString("\n")
    lazy val collate =
      Layer(for index <- 0 until d.w yield (for row <- rows yield row(index)).mkString)
    lazy val pixels = rows.map(_.seeThru).mkString // do not re-collate from Layers

  case class Layers(layers: Seq[Layer])(using d: Dim):
    lazy val size = layers.size
    lazy val transpose: Seq[Layer] =
      for index <- 0 until d.h yield Layer(for layer <- layers yield layer.rows(index))
    lazy val collated: Seq[Layer] = transpose.map(_.collate) // do not re-collate in Layer
    lazy val pixels = Layer(collated.map(_.pixels)).show

  // Seq(02, 22), Seq(11, 22), Seq(22, 12), Seq(00, 00)
  // Seq(02, 11, 22, 00), Seq(22, 22, 12, 00)
  // Seq(0120, 2120), Seq(2210, 2220)
  //         01              10

object Parsing:
  import DataDefs.*
  def parse(line: String)(using d: Dim) = Layers(
    line
      .grouped(d.size)
      .map(it => Layer(it.grouped(d.w).toSeq))
      .toSeq
  )

object Solving:
  import DataDefs.*
  def solve1(line: String)(using d: Dim) = Parsing
    .parse(line)
    .layers
    .minBy(_.count('0'))
    .part1

  def solve2(line: String)(using d: Dim) = Parsing.parse(line).pixels

object Testing:
  import DataDefs.*
  given d: Dim = Dim(2, 2)
  private lazy val lines = os.read.lines(os.pwd / "08.test.input.txt").head
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
  val x = "2220".seeThru
Testing.result1 // part 1: 4
Testing.result2 // part 2: 0110

object Main:
  import DataDefs.*
  given d: Dim = Dim(25, 6)
  lazy val line = os.read.lines(os.pwd / "08.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
Main.result1 // part 1: 2318
Main.result2 // part 2: AHFCB

// ./\..|..|.|___..__..|_\..
// |..|.|..|.|....|..|.|..|.
// |..|.|__|.|__..|....|__..
// |__|.|..|.|....|....|..|.
// |..|.|..|.|....|..|.|..|.
// |..|.|..|.|.....__..|__..
// AHFCB
