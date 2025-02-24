package aoc2019.day04

object Solving:
  def solve1(start: Int, end: Int) =
    (start to end).view
      .map(_.toString)
      .filter(str => (0 to 4).forall(index => str(index) <= str(index + 1)))
      .filter(str => (0 to 4).exists(index => str(index) == str(index + 1)))
      .size

  def solve2(start: Int, end: Int) =
    (start to end).view
      .map(_.toString)
      .filter(str => (0 to 4).forall(index => str(index) <= str(index + 1)))
      .filter: str =>
        (0 to 4).exists: index =>
          str(index) == str(index + 1) && str.count(_ == str(index)) == 2
      .size

@main
def run: Unit =
  println(Solving.solve1(156218, 652527)) // part 1: 1694
  println(Solving.solve2(156218, 652527)) // part 2: 1148
