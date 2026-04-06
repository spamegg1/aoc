package aoc2025.`06`

@main
def run: Unit =
  val cols = os.read
    .lines(os.pwd / "2025" / "06" / "06.input.txt")
    .map(_.toCharArray)
    .toList
    .transpose
  val colGroups = Vector.unfold(cols): remaining =>
    val (nextGroup, nextCols) = remaining.span(_.exists(_ != ' '))
    Option.when(remaining.nonEmpty)(nextGroup, nextCols.drop(1))

  val res = colGroups
    .map: colGroup =>
      val op   = colGroup.head.last
      val nums = colGroup.map(_.filter(_.isDigit).mkString.toLong)
      if op == '+' then nums.sum else nums.product
    .sum
  println(res)
