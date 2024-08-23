/*
--- Day 9: Explosives in Cyberspace ---
Wandering around a secure area,
you come across a datalink port to a new part of the network.
After briefly scanning it for interesting files,
you find one file in particular that catches your attention.
It's compressed with an experimental format, but fortunately,
the documentation for the format is nearby.

The format compresses a sequence of characters. Whitespace is ignored.
To indicate that some sequence should be repeated, a marker is added to the file,
like (10x2). To decompress this marker, take the subsequent 10 characters and
repeat them 2 times. Then, continue reading the file after the repeated data.
The marker itself is not included in the decompressed output.

If parentheses or other characters appear within the data referenced by a marker,
that's okay - treat it like normal data, not a marker,
and then resume looking for markers after the decompressed section.

For example:
  ADVENT contains no markers and decompresses to itself with no changes,
    resulting in a decompressed length of 6.
  A(1x5)BC repeats only the B a total of 5 times,
    becoming ABBBBBC for a decompressed length of 7.
  (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
  A(2x2)BCD(2x2)EFG doubles the BC and EF,
    becoming ABCBCDEFEFG for a decompressed length of 11.
  (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker,
    but because it's within a data section of another marker,
    it is not treated any differently from the A that comes after it.
    It has a decompressed length of 6.
  X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18),
    because the decompressed data from the (8x2) marker
    (the (3x3)ABC) is skipped and not processed further.

What is the decompressed length of the file (your puzzle input)? Don't count whitespace.

--- Part Two ---
Apparently, the file actually uses version two of the format.
In version two, the only difference is that markers within decompressed data
are decompressed. This, the documentation explains, provides much more
substantial compression capabilities, allowing many-gigabyte files to be
stored in only a few kilobytes.

For example:
  (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
  X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY,
    because the decompressed data from the (8x2) marker is then further decompressed,
    thus triggering the (3x3) marker twice for a total of six ABC sequences.
  (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times
  (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.

Unfortunately, the computer you brought probably doesn't have enough
memory to actually decompress the file; you'll have to come up with
another way to get its decompressed length.
What is the decompressed length of the file using this improved format?
 */
object Parsing:
  @annotation.tailrec
  def parse1(line: String)(acc: Int): Int =
    if line.isEmpty then acc
    else
      line match
        case s"$pre(${letters}x$copies)$post" =>
          val (decompress, rest) = (post.take(letters.toInt), post.drop(letters.toInt))
          parse1(rest)(acc + pre.length + decompress.length * copies.toInt)
        case _ => acc + line.length

  def parse2(line: String)(acc: Long): Long =
    if line.isEmpty then acc
    else
      line match
        case s"$pre(${letters}x$copies)$post" =>
          val (decompress, rest) = (post.take(letters.toInt), post.drop(letters.toInt))
          val decompressed = parse2(decompress)(0L) // deep recursion here
          parse2(rest)(acc + pre.length + decompressed * copies.toLong)
        case _ => acc + line.length

object Solving:
  def solve1(line: String) = Parsing.parse1(line)(0)
  def solve2(line: String) = Parsing.parse2(line)(0L)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "09.test.input.txt")
  lazy val result1 = lines map Solving.solve1
  lazy val result2 = lines map Solving.solve2
// Testing.result1 // part 1: 6,7,9,11,6,18,324,238
// Testing.result2 // part 2: 6,7,9,11,3,20,241920,445

object Main:
  private lazy val line = os.read.lines(os.pwd / "09.input.txt").head
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
// Main.result1 // part 1: 110346
// Main.result2 // part 2: 10774309173
