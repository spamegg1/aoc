/*
--- Day 15: Science for Hungry People ---
Today, you set out on the task of perfecting your milk-dunking cookie recipe.
All you have to do is find the right balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients.
You make a list of the remaining ingredients you could use to finish the recipe
(your puzzle input) and their properties per teaspoon:
  capacity (how well it helps the cookie absorb milk)
  durability (how well it keeps the cookie intact when full of milk)
  flavor (how tasty it makes the cookie)
  texture (how it improves the feel of the cookie)
  calories (how many calories it adds to the cookie)

You can only measure ingredients in whole-teaspoon amounts accurately,
and you have to be accurate so you can reproduce your results in the future.
The total score of a cookie can be found by adding up each of the properties
(negative totals become 0) and then multiplying together everything except calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon
(because the amounts of each ingredient must add up to 100) would result
in a cookie with the following properties:
  A capacity of 44*-1 + 56*2 = 68
  A durability of 44*-2 + 56*3 = 80
  A flavor of 44*6 + 56*-2 = 152
  A texture of 44*3 + 56*-1 = 76

Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
results in a total score of 62842880,
which happens to be the best score possible given these ingredients.
If any properties had produced a negative total,
it would have instead become zero, causing the whole score to multiply to zero.

Given the ingredients in your kitchen and their properties,
what is the total score of the highest-scoring cookie you can make?

--- Part Two ---
Your cookie recipe becomes wildly popular!
Someone asks if you can make another recipe that has exactly
500 calories per cookie (so they can use it as a meal replacement).
Keep the rest of your award-winning process the same
(100 teaspoons, same ingredients, same scoring system).

For example, given the ingredients above, if you had instead selected
40 teaspoons of butterscotch and 60 teaspoons of cinnamon
(which still adds to 100), the total calorie count would be 40*8 + 60*3 = 500.
The total score would go down, though: only 57600000,
the best you can do in such trying circumstances.

Given the ingredients in your kitchen and their properties, what is the total
score of the highest-scoring cookie you can make with a calorie total of 500?
 */
object DataDefs:
  case class Ingr(
      name: String,
      capacity: Long,
      durability: Long,
      flavor: Long,
      texture: Long,
      calories: Long
  )

object Parsing:
  import DataDefs.*
  private def parseLine(line: String) = line match
    case s"$nm: capacity $cap, durability $dur, flavor $fl, texture $tx, calories $cal" =>
      Ingr(nm, cap.toLong, dur.toLong, fl.toLong, tx.toLong, cal.toLong)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  private def getAllMeasures(ingrs: Seq[Ingr]): Seq[Seq[(Ingr, Long)]] =
    for // change this to 2 vars for Testing by commenting out w2, w3
      w1 <- 0L to 100L
      w2 <- 0L to 100L
      w3 <- 0L to 100L
      w4 = 100L - w1 - w2 - w3
    // w4 = 100L - w1
    yield ingrs.zip(Seq(w1, w2, w3, w4))
    // yield ingrs.zip(Seq(w1, w4))

  private def score(measures: Seq[(Ingr, Long)]): Long =
    val capacity = measures.map((ingr, wght) => ingr.capacity * wght).sum
    val durability = measures.map((ingr, wght) => ingr.durability * wght).sum
    val flavor = measures.map((ingr, wght) => ingr.flavor * wght).sum
    val texture = measures.map((ingr, wght) => ingr.texture * wght).sum
    capacity.max(0L) * durability.max(0L) * flavor.max(0L) * texture.max(0L)

  private def calories(measures: Seq[(Ingr, Long)]): Long =
    measures.map((ingr, wght) => ingr.calories * wght).sum

  def solve(pred: Seq[(Ingr, Long)] => Boolean)(lines: Seq[String]) =
    val ingrs = Parsing.parse(lines)
    val allMeasures = getAllMeasures(ingrs).filter(pred)
    val best = allMeasures.maxBy(score)
    score(best)

  val solve1 = solve(_ => true)
  val solve2 = solve(calories(_) == 500L)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "15" / "15.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 62842880
// Testing.result2 // part 2: 57600000

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "15" / "15.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 13882464
// Main.result2 // part 2: 11171160
