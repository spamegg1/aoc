/*
--- Day 21: Keypad Conundrum ---
As you teleport onto Santa's Reindeer-class starship,
The Historians begin to panic: someone from their search party is missing.
A quick life-form scan by the ship's computer reveals that when the missing
Historian teleported, he arrived in another part of the ship.

The door to that area is locked, but the computer can't open it;
it can only be opened by physically typing the door codes (your puzzle input)
on the numeric keypad on the door.

The numeric keypad has four rows of buttons: 789, 456, 123,
and finally an empty gap followed by 0A. Visually, they are arranged like this:

+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+

Unfortunately, the area outside the door is currently depressurized and
nobody can go near the door. A robot needs to be sent instead.

The robot has no problem navigating the ship and finding the numeric keypad,
but it's not designed for button pushing: it can't be told to push a specific
button directly. Instead, it has a robotic arm that can be controlled
remotely via a directional keypad.

The directional keypad has two rows of buttons:
  a gap / ^ (up) / A (activate) on the first row and
  < (left) / v (down) / > (right) on the second row.
Visually, they are arranged like this:

    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+

When the robot arrives at the numeric keypad,
its robotic arm is pointed at the A button in the bottom right corner.
After that, this directional keypad remote control must be used to
maneuver the robotic arm: the up / down / left / right buttons cause
it to move its arm one button in that direction, and the A button
causes the robot to briefly move forward, pressing the button being
aimed at by the robotic arm.

For example, to make the robot type 029A on the numeric keypad,
one sequence of inputs on the directional keypad you could use is:
  < to move the arm from A (its initial position) to 0.
  A to push the 0 button.
  ^A to move the arm to the 2 button and push it.
  >^^A to move the arm to the 9 button and push it.
  vvvA to move the arm to the A button and push it.

In total, there are three shortest possible sequences of button presses
on this directional keypad that would cause the robot to type
029A: <A^A>^^AvvvA, <A^A^>^AvvvA, and <A^A^^>AvvvA.

Unfortunately, the area containing this directional keypad remote control
is currently experiencing high levels of radiation and nobody can go near it.
A robot needs to be sent instead.

When the robot arrives at the directional keypad, its robot arm is
pointed at the A button in the upper right corner. After that, a second,
different directional keypad remote control is used to control this robot
(in the same way as the first robot, except that this one is typing on a
directional keypad instead of a numeric keypad).

There are multiple shortest possible sequences of directional keypad
button presses that would cause this robot to tell the first robot to
type 029A on the door. One such sequence is v<<A>>^A<A>AvA<^AA>A<vAAA>^A.

Unfortunately, the area containing this second directional keypad remote
control is currently -40 degrees! Another robot will need to be sent to
type on that directional keypad, too.

There are many shortest possible sequences of directional keypad button
presses that would cause this robot to tell the second robot to tell the
first robot to eventually type 029A on the door. One such sequence is
<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A.

Unfortunately, the area containing this third directional keypad remote
control is currently full of Historians, so no robots can find a clear path there.
Instead, you will have to type this sequence yourself.

Were you to choose this sequence of button presses, here are all of the
buttons that would be pressed on your directional keypad, the two robots'
directional keypads, and the numeric keypad:

<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
v<<A>>^A<A>AvA<^AA>A<vAAA>^A
<A^A>^^AvvvA
029A

In summary, there are the following keypads:
  One directional keypad that you are using.
  Two directional keypads that robots are using.
  One numeric keypad (on a door) that a robot is using.

It is important to remember that these robots are not designed for button pushing.
In particular, if a robot arm is ever aimed at a gap where no button is
present on the keypad, even for an instant, the robot will panic unrecoverably.
So, don't do that. All robots will initially aim at the keypad's A key, wherever it is.

To unlock the door, five codes will need to be typed on its numeric keypad.
For example:

029A
980A
179A
456A
379A

For each of these, here is a shortest sequence of button presses
you could type to cause the desired code to be typed on the numeric keypad:

029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A

The Historians are getting nervous; the ship computer doesn't remember whether
the missing Historian is trapped in the area containing a giant electromagnet
or molten lava. You'll need to make sure that for each of the five codes,
you find the shortest sequence of button presses necessary.

The complexity of a single code (like 029A) is equal to the res of
multiplying these two values:
  The length of the shortest sequence of button presses you need to type
    on your directional keypad in order to cause the code to be typed on
    the numeric keypad; for 029A, this would be 68.
  The numeric part of the code (ignoring leading zeroes); for 029A, this would be 29.

In the above example, complexity of the five codes can be found by calculating
68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379.
Adding these together produces 126384.

Find the fewest number of button presses you'll need to perform in order
to cause the robot in front of the door to type each code.
What is the sum of the complexities of the five codes on your list?

 */
package aoc2024.day21

import scalax.collection.edges.{DiEdge, DiEdgeImplicits} // for ~>
import scalax.collection.edges.labeled.LDiEdge
import scalax.collection.mutable.Graph

object DataDefs:
  type Num = Char

  enum Dir:
    case N, S, E, W
  import Dir.*

  // for infix sugar n1 ~> n2 :+ label where ~> is given by DiEdgeImplicits.
  extension (e: DiEdge[Num])
    def :+(dir: Dir) = new LDiEdge[Num, Dir]:
      def label: Dir  = dir
      def source: Num = e.source
      def target: Num = e.target

  val numPad = Graph.empty[Num, LDiEdge[Num, Dir]]
  // numPad ++= (Seq('A', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'), Seq(
  //   'A' ~> '0' :+ W,
  //   '0' ~> 'A' :+ E,
  //   'A' ~> '3' :+ N,
  //   '3' ~> 'A' :+ S,
  //   '0' ~> '2' :+ N,
  //   '2' ~> '0' :+ S,
  //   '3' ~> '2' :+ W,
  //   '2' ~> '3' :+ E,
  //   '3' ~> '6' :+ N,
  //   '6' ~> '3' :+ S,
  //   '2' ~> '1' :+ W,
  //   '1' ~> '2' :+ E,
  //   '2' ~> '5' :+ N,
  //   '5' ~> '2' :+ S,
  //   '1' ~> '4' :+ N,
  //   '4' ~> '1' :+ S,
  //   '6' ~> '5' :+ W,
  //   '5' ~> '6' :+ E,
  //   '6' ~> '9' :+ N,
  //   '9' ~> '6' :+ S,
  //   '5' ~> '4' :+ W,
  //   '4' ~> '5' :+ E,
  //   '5' ~> '8' :+ N,
  //   '8' ~> '5' :+ S,
  //   '4' ~> '7' :+ N,
  //   '7' ~> '4' :+ S,
  //   '9' ~> '8' :+ W,
  //   '8' ~> '9' :+ E,
  //   '8' ~> '7' :+ W,
  //   '7' ~> '8' :+ E
  // ))

  val dirPad = Graph.empty[Num, LDiEdge[Num, Dir]]
  // dirPad ++= (Seq('A', '<', '>', '^', 'v'), Seq(
  //   '<' ~> 'v' :+ E,
  //   'v' ~> '<' :+ W,
  //   'v' ~> '^' :+ N,
  //   '^' ~> 'v' :+ S,
  //   'v' ~> '>' :+ E,
  //   '>' ~> 'v' :+ W,
  //   '>' ~> 'A' :+ N,
  //   'A' ~> '>' :+ S,
  //   'A' ~> '^' :+ E,
  //   '^' ~> 'A' :+ W
  // ))

object Parsing:
  import DataDefs.*
  ???

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = 0L
  def solve2(lines: Seq[String]) = 0L

object Test:
  lazy val lines = os.read.lines(os.pwd / "2024" / "21" / "21.test.input.txt")
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val lines = os.read.lines(os.pwd / "2024" / "21" / "21.input.txt")
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 126384
  // println(Test.res2) // part 2:
  // println(Main.res1) // part 1:
  // println(Main.res2) // part 2:
