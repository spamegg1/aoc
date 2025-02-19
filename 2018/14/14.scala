package aoc2018.day14

object DataDefs:
  case class State(recipes: Vector[Int], elf1: Int, elf2: Int):
    def next: State =
      val rec1 = recipes(elf1)
      val rec2 = recipes(elf2)
      val sum  = rec1 + rec2
      val nextRecipes =
        if sum >= 10 then recipes.appended(sum / 10).appended(sum % 10)
        else recipes.appended(sum)
      val nextElf1 = (elf1 + 1 + rec1) % nextRecipes.length
      val nextElf2 = (elf2 + 1 + rec2) % nextRecipes.length
      State(nextRecipes, nextElf1, nextElf2)

  val start = State(Vector(3, 7), 0, 1)

object Solving:
  import DataDefs.*

  def solve1(count: Int) = Iterator
    .iterate(start)(_.next)
    .dropWhile(_.recipes.length < count + 10)
    .next()
    .recipes
    .drop(count)
    .take(10)
    .mkString

  def solve2(appear: Int) =
    val digits = appear.toString.map(_.asDigit)
    Iterator
      .iterate((start, -1)): (state, _) =>
        val index = state.recipes
          .indexOfSlice(digits, state.recipes.length - digits.length - 1)
        (state.next, index)
      .dropWhile(_._2 == -1)
      .next()
      ._2

object Test:
  lazy val res1 = Solving.solve1(2018)
  lazy val res2 = Solving.solve2(59414)

object Main:
  lazy val res1 = Solving.solve1(540391)
  lazy val res2 = Solving.solve2(540391)

@main
def run: Unit =
  println(Test.res1) // part 1: 5941429882
  println(Test.res2) // part 2: 2018
  println(Main.res1) // part 1: 1474315445
  println(Main.res2) // part 2: 20278122
