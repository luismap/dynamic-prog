import Utils.timer
import org.apache.log4j.Logger

import scala.collection.mutable

object TravellerTwoD  extends  App {

  val logger = Logger.getLogger(this.getClass.getName)

  /**
   * scenario.
   * traveller in 2 D matrix
   * - begin at 0,0
   * - goal, travel to bottom right corner
   * - can only move down or right
   *
   * question?
   *  how many ways can you travel from start to goal on a grid of
   *  dims(m,n)
   *  in general think about
   *  time complexity = to how many calls to a function
   *  space complexity = the depth of the tree(visualize the recursive calls as tree)
   */

    //O(2**(m+n)) time
    //O(m+n) space
  def travellerBrute(m: Int, n: Int): Long = {
    if (m == 0 | n == 0 ) 0
    else if (m == 1 & n == 1) 1
    else travellerBrute(m - 1, n) +  travellerBrute(m , n - 1)
  }

  val stash = mutable.HashMap.empty[String, Long]
  stash("0,0") = 0
  stash("1,0") = 0
  stash("1,1") = 1

  def travellerOptimize(m: Int, n: Int): Long = {
    val key = s"$m,$n"
    val reverseKey = s"$n,$m"
    if (stash.contains(key)) stash(key)
    else if (stash.contains(reverseKey)) stash(reverseKey)
    else if (m == 0 || n == 0 ) 0
    else if (m == 1 && n == 1) 1
    else {
      stash(key) = travellerOptimize(m - 1, n) +  travellerOptimize(m , n - 1)
      stash(key)
    }
  }

  val (m,n) = (18,18)
  val tbrute = timer(travellerBrute(m,n), "traveler-brute", logger)
  val toptimize = timer(travellerOptimize(m,n), "traveller-optimize", logger)

  logger.info(s"[travellerBrute] steps in $m, $n matrix = $tbrute")
  logger.info(s"[travellerOptimize] steps in $m, $n matrix = $toptimize")

  //stash.foreach(println)
}
