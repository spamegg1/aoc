package aoc2016.day14

import org.apache.commons.codec.digest.DigestUtils.md5Hex
import scala.util.boundary, boundary.break

object DataDefs:
  val digits  = ((0 to 9) ++ ('a' to 'z')) map (_.toString)
  val triples = digits map (_ * 3)

  extension (hash: String)
    def isCandidate = triples.exists(hash.contains(_))
    def whichCandidate = hash.zipWithIndex
      .find((char, index) => triples.exists(hash.drop(index).startsWith(_)))
      .get
      ._1
      .toString * 5

object Solving:
  import DataDefs.*

  def solve(salt: String, keys: Int, stretch: Int) =
    val hashes   = collection.mutable.Map[Int, String]()
    var index    = 0
    var keyCount = 0

    while keyCount < keys do
      val hash = hashes.get(index) match
        case None =>
          var hash = md5Hex(salt + index.toString)
          for _ <- 0 until stretch do hash = md5Hex(hash)
          hashes.update(index, hash)
          hash
        case Some(value) => value

      if hash.isCandidate then
        val five = hash.whichCandidate
        boundary:
          for nextIndex <- index + 1 to index + 1000 do
            val nextHash = hashes.get(nextIndex) match
              case None =>
                var nextHash = md5Hex(salt + nextIndex.toString)
                for _ <- 0 until stretch do nextHash = md5Hex(nextHash)
                hashes.update(nextIndex, nextHash)
                nextHash
              case Some(value) => value
            if nextHash.contains(five) then
              keyCount += 1
              break()
      index += 1

    index - 1

object Test:
  lazy val res1 = Solving.solve("abc", 64, 0)
  lazy val res2 = Solving.solve("abc", 64, 2016)

object Main:
  lazy val res1 = Solving.solve("qzyelonm", 64, 0)
  lazy val res2 = Solving.solve("qzyelonm", 64, 2016)

@main
def run: Unit =
  println(Test.res1) // part 1: 22728
  println(Test.res2) // part 2: 22551 kind of slow
  println(Main.res1) // part 1: 15168
  println(Main.res2) // part 2: 20864 kind of slow
