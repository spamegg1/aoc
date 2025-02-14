object DataDefs:
  type Label  = String
  type Weight = Int
  type Tree   = Map[Label, Node]

  case class Node(label: Label, weight: Weight, subtree: Seq[Label]):
    def weights(using tree: Tree): Seq[Weight] = subtree.map(tree).map(_.totalWeight)
    def totalWeight(using tree: Tree): Weight  = weight + weights.sum
    def isBalanced(using tree: Tree): Boolean  = weights.distinct.size == 1

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): (Label, Node) = line match
    case s"$l ($w) -> $sub" => l -> Node(l, w.toInt, sub.split(", ").toSeq)
    case s"$l ($w)"         => l -> Node(l, w.toInt, Seq())

  def parse(lines: Seq[String]): Tree = lines.map(parseLine).toMap

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]): Label =
    val tree = Parsing.parse(lines)
    tree.keys.find(label => !tree.values.exists(_.subtree.contains(label))).get

  @annotation.tailrec
  private def traverse(node: Node)(using tree: Tree): Weight =
    val subs = node.subtree.map(tree)
    subs.find(!_.isBalanced) match
      case Some(value) => traverse(value)
      case None =>
        val weights                  = subs.groupBy(_.totalWeight)
        val (wrongWeight, wrongTree) = weights.find((w, seq) => seq.size == 1).get
        val wrongNode                = wrongTree.head
        val rightWeight              = weights.find((w, seq) => seq.size > 1).get._1
        wrongNode.weight + (rightWeight - wrongWeight)

  def solve2(lines: Seq[String]): Weight =
    given tree: Tree = Parsing.parse(lines)
    val root         = tree(solve1(lines))
    traverse(root)

object Test:
  lazy val file  = os.pwd / "2017" / "07" / "07.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: tknk
// Test.res2 // part 2: 60

object Main:
  lazy val file  = os.pwd / "2017" / "07" / "07.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: airlri
// Main.res2 // part 2: 1206
