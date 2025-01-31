/*
--- Day 14: Chocolate Charts ---
You finally have a chance to look at all of the produce moving around.
Chocolate, cinnamon, mint, chili peppers, nutmeg, vanilla...
the Elves must be growing these plants to make hot chocolate!
As you realize this, you hear a conversation in the distance.
When you go to investigate, you discover two Elves in what appears
to be a makeshift underground kitchen/laboratory.

The Elves are trying to come up with the ultimate hot chocolate recipe;
they're even maintaining a scoreboard which tracks the quality
score (0-9) of each recipe.

Only two recipes are on the board: the first recipe got a
score of 3, the second, 7. Each of the two Elves has a current recipe:
  the first Elf starts with the first recipe, and
  the second Elf starts with the second recipe.

To create new recipes, the two Elves combine their current recipes.
This creates new recipes from the digits of the sum of the current
recipes' scores. With the current recipes' scores of 3 and 7, their
sum is 10, and so two new recipes would be created: the first with
score 1 and the second with score 0. If the current recipes' scores
were 2 and 3, the sum, 5, would only create one recipe (with a score of 5)
with its single digit.

The new recipes are added to the end of the scoreboard in
the order they are created.
So, after the first round, the scoreboard is 3, 7, 1, 0.

After all new recipes are added to the scoreboard,
each Elf picks a new current recipe.
To do this, the Elf steps forward through the scoreboard a number of recipes
equal to 1 plus the score of their current recipe.
So, after the first round, the first Elf moves forward 1 + 3 = 4 times,
while the second Elf moves forward 1 + 7 = 8 times.
If they run out of recipes, they loop back around to the beginning.
After the first round, both Elves happen
to loop around until they land on the same recipe
that they had in the beginning;
in general, they will move to different recipes.

Drawing the first Elf as parentheses and the second Elf as square brackets,
they continue this process:

(3)[7]
(3)[7] 1  0
 3  7  1 [0](1) 0
 3  7  1  0 [1] 0 (1)
(3) 7  1  0  1  0 [1] 2
 3  7  1  0 (1) 0  1  2 [4]
 3  7  1 [0] 1  0 (1) 2  4  5
 3  7  1  0 [1] 0  1  2 (4) 5  1
 3 (7) 1  0  1  0 [1] 2  4  5  1  5
 3  7  1  0  1  0  1  2 [4](5) 1  5  8
 3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6
 3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7
 3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7
 3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9
 3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2

The Elves think their skill will improve after making a few recipes
(your puzzle input).
However, that could take ages; you can speed this up
considerably by identifying
the scores of the ten recipes after that. For example:
  If the Elves think their skill will improve after making 9 recipes,
    the scores of the ten recipes after the first nine on
    the scoreboard would be 5158916779 (highlighted in the last line
    of the diagram).
  After 5 recipes, the scores of the next ten would be 0124515891.
  After 18 recipes, the scores of the next ten would be 9251071085.
  After 2018 recipes, the scores of the next ten would be 5941429882.

What are the scores of the ten recipes immediately
after the number of recipes in your puzzle input?

--- Part Two ---
As it turns out, you got the Elves' plan backwards.
They actually want to know how many recipes appear
on the scoreboard to the left of the first recipes
whose scores are the digits from your puzzle input.
  51589 first appears after 9 recipes.
  01245 first appears after 5 recipes.
  92510 first appears after 18 recipes.
  59414 first appears after 2018 recipes.

How many recipes appear on the scoreboard to
the left of the score sequence in your puzzle input?
 */
import collection.mutable.ArrayDeque

object DataDefs:
  case class Recipes(recipes: ArrayDeque[Int], elf1: Int, elf2: Int, size: Int):
    val total = recipes(elf1) + recipes(elf2)
    def newRecipes = if total < 10 then Seq(total) else Seq(total / 10, total % 10)
    val nextSize = size + (if total < 10 then 1 else 2)
    def nextElf1 = (elf1 + recipes(elf1) + 1) % nextSize
    def nextElf2 = (elf2 + recipes(elf2) + 1) % nextSize
    def nextRecipes = recipes.appendAll(newRecipes)
    def next = Recipes(nextRecipes, nextElf1, nextElf2, nextSize)
    def show = recipes.mkString

object Solving:
  import DataDefs.*

  def solve1(start: ArrayDeque[Int], count: Int) =
    var recipes = Recipes(start, 0, 1, start.size)
    while recipes.size < count + 10 do recipes = recipes.next
    recipes.recipes.drop(count).take(10).mkString

  def solve2(start: ArrayDeque[Int], appear: String) =
    var recipes = Recipes(start, 0, 1, start.size)
    while !recipes.show.contains(appear) do recipes = recipes.next
    recipes.show.indexOf(appear)

object Test:
  import DataDefs.*
  var recipes = Recipes(ArrayDeque(3, 7), 0, 1, 2)
  for _ <- 1 to 15 do recipes = recipes.next
  lazy val res1 = Solving.solve1(ArrayDeque(3, 7), 2018)
  lazy val res2 = Solving.solve2(ArrayDeque(3, 7), "59414")
// Test.recipes // 3710[1]012(4)51589167792
// Test.res1 // part 1: 5941429882
// Test.res2 // part 2: 2018

object Main:
  lazy val res1 = Solving.solve1(ArrayDeque(3, 7), 540391)
  lazy val res2 = Solving.solve2(ArrayDeque(3, 7), "540391")
// Main.res1 // part 1: 1474315445
// Main.res2 // part 2: 20278122
